# Changelog for `haal-models`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.1.2.0 - 2026-10-03

### Changed
- All models are regenerated with the table-based `haal-gen`, which builds
  them with `Haal.Automaton.MealyAutomaton.mkMealyAutomatonTable` instead of
  one equation per transition. Types, state numbering, and transitions are
  unchanged, and stepping a model is as fast as before. The package builds
  in about half the time at `-O1` and a third at `-O0`. This requires the
  `haal` release that adds `mkMealyAutomatonTable`. Bumped dependency 
  to `haal >= 0.6 && haal < 0.7`.

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
