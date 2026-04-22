# Changelog for `haal-models`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.1.0 - 2026-04-22

### Added
- `tasty-bench`-based benchmark suite in `bench/` covering end-to-end
  learning on real protocol models (MQTT, TLS, TCP) and scaling analysis
  across model sizes. Run with `stack bench haal-models`.

### Changed
- Bumped `haal` dependency bound to `>= 0.5.0.0 && < 0.6`.

## 0.1.0.0 - 2026-03-21

- Initial release. Pre-built Mealy automaton models for DTLS, MQTT, TCP,
  and TLS protocols generated from DOT files via `haal-gen`.
