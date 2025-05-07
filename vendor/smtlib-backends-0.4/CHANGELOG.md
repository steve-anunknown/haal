# Changelog

All notable changes to the smtlib-backends library will be documented in this
file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP versioning](https://pvp.haskell.org/).

The same stands for the changelogs of
[smtlib-backends-tests](smtlib-backends-tests/CHANGELOG.md),
[smtlib-backends-process](smtlib-backends-process/CHANGELOG.md) and
[smtlib-backends-z3](smtlib-backends-z3/CHANGELOG.md), except the version
numbers simply follow that of `smtlib-backends`.

## v0.4 _(2024-05-28)_

### Changed

- **(breaking change)** stop changing the default of the `produce-models` option.

## v0.3 _(2023-02-03)_

### Added
- **(breaking change)** add a datatype `Backends.QueuingFlag` to set the queuing
  mode
  - the `initSolver` function now takes this datatype as argument instead of a
    boolean
- **(breaking change)** add a `send_` method to the `Backends.Backend` datatype
  for sending commands with no output
- add a `Backends.flushQueue` function for forcing the content of the queue to
  be evaluated

### Changed
- **(breaking change)** make the queuing functions thread-unsafe but faster

## v0.2 _(2022-12-16)_

### Changed
- split the `Process` module into its own library
- rename `SMTLIB.Backends`'s `ackCommand` to `command_`
- improve read-me

### Removed
- remove logging abilities
  - the user can always surround `command` or `command_` with their own logging
    functions
