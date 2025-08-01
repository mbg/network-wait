# Changelog for network-wait

## 0.4.0

- Fixed [an issue](https://github.com/mbg/network-wait/issues/19) where under some circumstances an attempt to connect to a socket could become stuck for a long time or possibly forever. A timeout is now applied to all connection attempts made by functions in the `Network.Wait` module to ensure that the checks terminate in a reasonable amount of time. Contributed by [@thomasjm](https://github.com/thomasjm) in [#20](https://github.com/mbg/network-wait/pull/20).
- Added a new `recoveringWithStatus` function which provides the `RetryStatus` to the action.

## 0.3.0

- Functions in the `Network.Wait.PostgreSQL` module are now overloaded to accept different types of connection information. In addition to the previously supported `ConnectInfo` type, the function now also accept connection strings in the form of `ByteString` values.

## 0.2.0

- Add `Network.Wait.Redis` module with functions to wait for Redis servers to become ready to accept connections. This module and its dependency on `hedis` are not enabled by default. The `network-wait:redis` flag must be enabled for this package's Redis support.
- Generalise the `recoveringWith` function to allow it to return the result of a computation.
- The functions for TCP/Sockets now return the `Socket` if they are successful.
- The functions for PostgreSQL now return the `Connection` if they are successful.

## 0.1.2

- Compatibility with GHC 8.2 and Stack LTS 11

## 0.1.1

- Add `Network.Wait.PostgreSQL` module with functions to wait for PostgreSQL servers to become ready to accept connections. This module and its dependency on `postgresql-simple` are not enabled by default. The `network-wait:postgres` flag must be enabled for this package's PostgreSQL support.

## 0.1.0

- First release
