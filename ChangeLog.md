# Changelog for network-wait

## 0.1.2

- Compatibility with GHC 8.2 and Stack LTS 11

## 0.1.1

- Add `Network.Wait.PostgreSQL` module with functions to wait for PostgreSQL servers to become ready to accept connections. This module and its dependency on `postgresql-simple` are not enabled by default. The `network-wait:postgres` flag must be enabled for this package's PostgreSQL support.

## 0.1.0

- First release
