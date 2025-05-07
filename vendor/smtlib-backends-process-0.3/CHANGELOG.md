# Changelog

All notable changes to the smtlib-backends-process library will be documented in
this file.

## v0.3 _(2023-02-03)_

### Added
- add tests for documenting edge cases of the backends
  - check that we can pile up procedures for exiting a process
  - what happens when sending an empty command
  - what happens when sending a command not producing any output
- add `Process.defaultConfig`
- add `std_err` field in `Config`: the user may now specifiy how to create the
  handle for the error channel

### Changed
- make the test-suite compatible with `smtlib-backends-0.3`
- **(breaking change)** use `process` instead of `typed-process` to manage the underlying process
  - change the definition of the `Process.Handle` datatype accordingly
  - remove `Process.wait`
  - there is now a single example in the test-suite showing how to 
    manage the underlying process and its I/O channels
- improve error messages inside `Process.toBackend`
    
### Removed
- removed `Process.wait`
- **(breaking change)** removed logging capabilities, this is now on the user to
  implement (see also the `underlyingProcess` example)
  - remove `Config`'s `reportError` field
  - remove `Handle`'s `errorReader` field
- **(breaking change)** removed `Data.Default` instance of `Config`

## v0.2 _(2022-12-16)_

### Added
- made `Config` an instance of the `Default` typeclass
- add usage examples in the test-suite

### Changed
- split `smtlib-backends`'s `Process` module into its own library
- move the logger function inside the `Config` datatype
- don't prefix error messages with `[stderr]`
- make compatible with `smtlib-backends-0.2`
- improve documentation
