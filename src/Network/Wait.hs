-------------------------------------------------------------------------------
-- network-wait
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains computations which wait for some networked service
-- to become available, subject to some retry policy from "Control.Retry".
-- The `waitSocketWith` function is the most general function exported by
-- this module, but several variants exist for convenience. You may wish
-- to start out with e.g. `waitTcp` or `waitSocket` initially and move
-- on to the more feature-rich variants if you need their functionality.
module Network.Wait (
    -- * TCP
    waitTcp,
    waitTcpVerbose,
    waitTcpVerboseFormat,
    waitTcpWith,

    -- * Sockets
    waitSocket,
    waitSocketVerbose,
    waitSocketVerboseFormat,
    waitSocketWith,

    -- * Utility
    recoveringWith
) where

-------------------------------------------------------------------------------

import Control.Exception (throwIO)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
-- Only needed for base < 4.11, redundant otherwise
import Data.Semigroup
import System.IO.Error
import System.Timeout

import Network.Socket

-------------------------------------------------------------------------------

-- | Each individual connect attempt needs a timeout to prevent it from hanging
-- indefinitely. This policy allows us to make that timeout length adaptive,
-- based on the 'RetryStatus' of the outer retry policy.
--
-- Thus, the first attempt to connect will have a short timeout (currently 100ms),
-- and then successive attempts will get longer timeouts via "FullJitter" backoff.
-- The goals of this are twofold:
--
-- 1) If a connect call hangs during the first few attempts, it is timed out quickly
-- and re-attempted, so on a healthy network you aren't penalized too much by the hang.
-- The outer retry policy can control the time between attempts, so the user can set
-- it high enough to make this be the case.
--
-- 2) If the network is slow, we will eventually reach the maximum timeout of 3 seconds,
-- which should be long enough. Note that the popular wait-for script uses 1 second
-- timeouts, so this is extra conservative:
-- https://github.com/eficode/wait-for/blob/7586b3622f010808bb2027c19aaf367221b4ad54/wait-for#L72
connectRetryPolicy :: MonadIO m => RetryPolicyM m
connectRetryPolicy = capDelay (3_000_000) (fullJitterBackoff 100_000)

-- | `waitTcp` @retryPolicy hostName serviceName@ is a variant of `waitTcpWith`
-- which does not install any additional handlers.
--
-- > waitTcp retryPolicyDefault "localhost" "80"
waitTcp
    :: (MonadIO m, MonadMask m)
    => RetryPolicyM m -> HostName -> ServiceName -> m Socket
waitTcp = waitTcpWith []

-- | `waitTcpVerbose` @outputHandler retryPolicy addrInfo@ is a variant
-- of `waitTcpVerboseFormat` which catches all exceptions derived from
-- `SomeException` and formats retry attempt information using `defaultLogMsg`
-- before passing the resulting `String` to @out@.
--
-- > waitTcpVerbose putStrLn retryPolicyDefault "localhost" "80"
waitTcpVerbose
    :: (MonadIO m, MonadMask m)
    => (String -> m ()) -> RetryPolicyM m -> HostName -> ServiceName
    -> m Socket
waitTcpVerbose out =
    waitTcpVerboseFormat @SomeException $
    \b ex st -> out $ defaultLogMsg b ex st

-- | `waitTcpVerboseFormat` @outputHandler retryPolicy addrInfo@ is a
-- variant of `waitTcpWith` which installs an extra handler based on
-- `logRetries` which passes status information for each retry attempt
-- to @outputHandler@.
--
-- > waitTcpVerboseFormat @SomeException
-- >      (\b ex st -> putStrLn $ defaultLogMsg b ex st)
-- >      retryPolicyDefault "localhost" "80"
waitTcpVerboseFormat
    :: forall e m . (MonadIO m, MonadMask m, Exception e)
    => (Bool -> e -> RetryStatus -> m ())
    -> RetryPolicyM m
    -> HostName
    -> ServiceName
    -> m Socket
waitTcpVerboseFormat out = waitTcpWith [h]
    where h = logRetries (const $ pure True) out

-- | `waitTcpWith` @extraHandlers retryPolicy hostName serviceName@ is a
-- variant of `waitSocketWith` which constructs a suitable `AddrInfo` value
-- for a TCP socket from @hostName@ and @serviceName@.
waitTcpWith
    :: (MonadIO m, MonadMask m)
    => [RetryStatus -> Handler m Bool]
    -> RetryPolicyM m -> HostName -> ServiceName -> m Socket
waitTcpWith hs policy host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr <- head <$> liftIO (getAddrInfo (Just hints) (Just host) (Just port))
    waitSocketWith hs policy addr

-- | `waitSocket` @retryPolicy addrInfo@ is a variant of `waitSocketWith` which
-- does not install any additional exception handlers.
waitSocket
    :: (MonadIO m, MonadMask m)
    => RetryPolicyM m -> AddrInfo -> m Socket
waitSocket = waitSocketWith []

-- | `waitSocketVerbose` @outputHandler retryPolicy addrInfo@ is a variant
-- of `waitSocketVerboseFormat` which catches all exceptions derived from
-- `SomeException` and formats retry attempt information using `defaultLogMsg`
-- before passing the resulting `String` to @out@.
waitSocketVerbose
    :: (MonadIO m, MonadMask m)
    => (String -> m ()) -> RetryPolicyM m -> AddrInfo -> m Socket
waitSocketVerbose out =
    waitSocketVerboseFormat @SomeException $
    \b ex st -> out $ defaultLogMsg b ex st

-- | `waitSocketVerboseFormat` @outputHandler retryPolicy addrInfo@ is a
-- variant of `waitSocketWith` which installs an extra handler based on
-- `logRetries` which passes status information for each retry attempt
-- to @outputHandler@.
waitSocketVerboseFormat
    :: forall e m . (MonadIO m, MonadMask m, Exception e)
    => (Bool -> e -> RetryStatus -> m ())
    -> RetryPolicyM m
    -> AddrInfo
    -> m Socket
waitSocketVerboseFormat out = waitSocketWith [h]
    where h = logRetries (const $ pure True) out

-- | `waitSocketWith` @extraHandlers retryPolicy addrInfo@ will attempt to
-- connect to @addrInfo@. If the connection fails, @retryPolicy@ is used
-- to determine whether (and how often) this function should attempt to
-- retry establishing the connection. By default, this function will
-- retry after all exceptions (except for those given by
-- `skipAsyncExceptions`). This behaviour may be customised with
-- @extraHandlers@ which are installed after `skipAsyncExceptions`, but
-- before the default exception handler. The @extraHandlers@ may also
-- be used to report retry attempts to e.g. the standard output or a
-- logger.
waitSocketWith
    :: (MonadIO m, MonadMask m)
    => [RetryStatus -> Handler m Bool] -> RetryPolicyM m -> AddrInfo
    -> m Socket
waitSocketWith hs policy addr =
    recoveringWith hs policy $ \retryStatus ->
    -- all of the networking code runs in IO
    liftIO $
    -- we want to make sure that we close the socket after every attempt;
    -- `bracket` will re-throw any error afterwards
    bracket initSocket close $ \sock -> do
        connectTimeoutUs <- (getRetryPolicyM connectRetryPolicy) retryStatus >>= \case
            Nothing -> throwIO $ userError "Timeout in connect attempt"
            Just us -> pure us

        timeout connectTimeoutUs (connect sock (addrAddress addr)) >>= \case
            Nothing -> throwIO $ userError "Timeout in connect attempt"
            Just () -> pure sock
    where
        initSocket =
            socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- | `recoveringWith` @extraHandlers retryPolicy action@ will attempt to
-- run @action@. If the @action@ fails, @retryPolicy@ is used
-- to determine whether (and how often) this function should attempt to
-- retry @action@. By default, this function will retry after all
-- exceptions (except for those given by `skipAsyncExceptions`). This
-- behaviour may be customised with @extraHandlers@ which are installed
-- after `skipAsyncExceptions`, but before the default exception handler.
-- The @extraHandlers@ may also be used to report retry attempts to e.g.
-- the standard output or a logger.
recoveringWith
    :: (MonadIO m, MonadMask m)
    => [RetryStatus -> Handler m Bool] -> RetryPolicyM m -> (RetryStatus -> m a) -> m a
recoveringWith hs policy action =
    -- apply the retry policy to the following code, with the combinations of
    -- the `skipAsyncExceptions`, given, and default handlers. The order of
    -- the handlers matters as they are checked in order.
    recovering policy (skipAsyncExceptions <> hs <> [defHandler]) $
        action
    where
        -- our default handler, which works with any exception derived from
        -- `SomeException`, and signals that we should retry if allowed by
        -- the retry policy
        defHandler _ = Handler $ \(_ :: SomeException) -> pure True

-------------------------------------------------------------------------------
