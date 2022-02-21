# network-wait

A lightweight Haskell library for waiting on networked services to become available. This is useful if you are e.g. building a web application which relies on a database server to be available, but which may not be immediately available on application startup.

## Example

All functions provided by this library work with retry policies from [`Control.Retry`](https://hackage.haskell.org/package/retry) which provide good control over the retry behaviour. To wait for a PostgreSQL server to become available on the same machine:

```haskell
import Control.Retry
import Network.Wait

main :: IO ()
main = do
    waitTcp retryPolicyDefault "localhost" "5432"
    putStrLn "Yay, the server is available!"
```

The haddock documentation for this library contains more examples.

## See also

- [wait-for](https://github.com/eficode/wait-for) is a popular shell script with the same objectives as this library.
- The [port-utils](https://hackage.haskell.org/package/port-utils) package has some functions for waiting on TCP servers to become available, with a fixed timeout.
