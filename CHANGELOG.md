# Changelog for `haal`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.4.0.1 - 2026-03-17

### Added
- Optional `liquid` Cabal flag (`--flag haal:liquid`) to enable LiquidHaskell
  verification without requiring it as a dependency for normal builds.

### Verified
- `Haal.BlackBox`: `walk` produces outputs of length equal to the input length.
- `Haal.Learning.LMstar`: `ObservationTable` invariant that all entries in
  `mappingT` map to non-empty output lists, preserved across `updateMap`,
  `makeConsistent`, `makeClosed`, and `initializeOT`.

## 0.4.0.0 - 2026-03-16

### Changed 
- Changed the types of oracles' constructors from `<Oracle>` to `Either String <Oracle>`
  where `String` is an error message indicating invalid values to `<OracleConfig>`.
  Now, for example, instead of `oracle = mkWMethod (WMethodConfig 2)`, one should either 
  pattern match with `case` or do `oracle = either error id (mkWMethod (WMethodConfig 2))`.

## 0.3.0.0 - 2026-03-11

### Changed
- `SUL` typeclass no longer takes `i` and `o` as class parameters; they are now
  universally quantified in the method signatures. Instances should drop `i o`
  from their instance heads: `instance SUL MyType IO` instead of
  `instance SUL MyType IO Input Output`.
- `Automaton` typeclass likewise drops `i` and `o` from its class head.
  All constraint occurrences `(Automaton aut s i o)` become `(Automaton aut s)`.

## 0.2.0.0 - 2026-03-05

### Added
- `stepPure`, `walkPure`, `resetPure` exported from `Haal.BlackBox`
- `Config` record types for all equivalence oracles: `WMethodConfig`, `WpMethodConfig`,
  `RandomWalkConfig`, `RandomWordsConfig`, `RandomWMethodConfig`, `RandomWpMethodConfig`
- `mkCombinedOracle` smart constructor for `CombinedOracle`
- `randomWordsConfig` accessor for `RandomWords`
- `mealyDelta`, `mealyLambda` as explicit named exports from `Haal.Automaton.MealyAutomaton`

### Changed
- All oracle constructors now take a `Config` record instead of positional arguments:
  `mkWMethod :: WMethodConfig -> WMethod`, `mkWpMethod :: WpMethodConfig -> WpMethod`, etc.
- `mkRandomWMethod` and `mkRandomWpMethod` now take a `Config` record instead of
  positional arguments (also fixes an argument-order bug in the old interface)

### Removed
- `mealyStep` from `Haal.Automaton.MealyAutomaton`; use `stepPure` from `Haal.BlackBox`
- `mooreStep` from `Haal.Automaton.MooreAutomaton`; use `stepPure` from `Haal.BlackBox`
- Raw constructor exports (`MealyAutomaton (..)`, `WMethod (..)`, `WpMethod (..)`,
  `RandomWalk (..)`, `RandomWords (..)`, `CombinedOracle (..)`, `LMstar (..)`);
  use the corresponding `mk`-prefixed smart constructors instead

## 0.1.0.0 - 2025-12-02

- Initial release of `haal`.
    - Support for Mealy Automata and DFAs.
    - One learner for Mealy Automata and DFAs with 2 configurations. 
        - LStar.
        - LPlus.
    - Basic equivalence oracles. 
    - Examples that showcase usage of the library.
