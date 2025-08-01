# Changelog for network-wait

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
