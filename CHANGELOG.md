# Changelog for `haal`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.6.1.1 - 2026-10-09

### Fixed
- The W-method and the Wp-method (and their random variants) did not test a
  hypothesis with a single state. Such a hypothesis has an empty characterizing
  set, so the W-method and the Wp-method generated no test words at all and
  accepted it without testing, at any depth, and the random Wp-method crashed
  with `Set.elemAt: index out of range`. This happens whenever no single input
  distinguishes the states of the SUL, so learning stopped after the first
  hypothesis with a wrong one-state model. Test words now end with the empty
  word when the characterizing set is empty, which still checks the outputs
  along the rest of the word.
- The second phase of the Wp-method started from the wrong prefixes: the state
  cover without the transition cover instead of the transition cover without
  the state cover. Depth `k` therefore only accounted for about `k - 1` extra
  states. Learned models can now be correct at a lower depth: on four
  `haal-models` protocol models, depth 1 now learns three of them correctly
  (before: none) and depth 2 all four (before: three).

## 0.6.1.0 - 2026-10-05

### Added
- `Haal.BlackBox.query` runs a single query: it resets the SUL, then walks it
  over the given inputs and returns the outputs.

### Fixed
- The SUL is now reset before every membership query and every equivalence
  test case. Before, queries only started from the initial state because the
  library kept returning to the same SUL value. That works for pure automata,
  but a SUL that wraps a stateful system (a running process, a socket) was
  queried from whatever state the previous query left it in, so learning could
  produce wrong models or never terminate. Learning from an automaton that is
  not in its initial state is fixed too.
- The documentation of `Haal.BlackBox.FiniteOrd` said `(Ord, Bounded)` instead
  of `(Ord, Finite)`.

### Changed
- Simplified the examples.

## 0.6.0.0 - 2026-10-03

### Added
- `Haal.Automaton.MealyAutomaton.mkMealyAutomatonTable` builds a Mealy
  automaton with states `0 .. n - 1` from a transition table and an output
  table, each encoded as a `String`, and validates both tables.
- `Haal.Dot.MealyTable` and `Haal.Dot.mealyTable` encode a `ParsedMealy` in
  that table form, rejecting automata with missing or conflicting
  transitions.

### Fixed
- `Haal.Dot.parseDot` again orders states, inputs, and outputs by first
  appearance, as documented. Since 0.5.0.0 it sorted them by name, which
  changed the constructor order and state numbering of generated modules.

### Changed (breaking)
- `Haal.Experiment.Learner` drops its state parameter (`Learner l aut`);
  learners now always produce automata with `Int` states.

### Changed
- `haal-gen` (`Haal.Dot.generateModule`) emits the transition and output
  functions as string-literal tables built with `mkMealyAutomatonTable`,
  instead of one equation per transition, and lists every transition in a
  comment. Generated modules have the same types and behaviour, but compile
  about 2-3 times faster. `generateModule` now rejects incomplete or
  nondeterministic automata, instead of generating functions that fail at
  runtime.
- `Haal.BlackBox.distinguish` returns `[]` immediately when both states are
  equal, instead of exploring the product automaton first. Results are
  unchanged. Its LiquidHaskell spec now states that equal states yield an
  empty word.

### Removed (breaking)
- `Haal.BlackBox.StateID` type alias. Learned automata use `Int` states
  directly, as `haal-gen` and `haal-models` already do; replace any use of
  `StateID` with `Int`.
- `Haal.Experiment.pairwiseWalk` and `Haal.Experiment.execute` are no longer
  exported; use `findCex` instead.

## 0.5.0.0 - 2026-04-22

### Fixed
- `Haal.Learning.LMstar.equivalenceClasses` now iterates over `Sm` only,
  matching Definition 2 of Shahbaz & Groz, "Inferring Mealy Machines".
  Previously it iterated over `Sm ∪ Sm·I`, which could pick a representative
  from `Sm·I` whose `rep++[i]` was not in the observation table, causing
  `makeHypothesis` to fail on many real protocol models (DTLS, medium MQTT)
  with `"invariant violation — makeHypothesis failed on closed consistent table"`.

### Changed (breaking)
- `Haal.Learning.LMstar.otIsConsistent` tightens its output constraint
  from `Eq o` to `Ord o` to support Map-based row grouping.

### Changed
- `Haal.Learning.LMstar.otIsConsistent` groups prefixes by row signature
  before pairwise comparison, reducing the worst-case pair enumeration.
- `Haal.Dot.parseDot` uses `Set.fromList` instead of `nub` for deduplication
  (O(n²) → O(n log n)).

### Added
- `tasty-bench`-based benchmark suite in `bench/` covering BlackBox
  operations, W-method / Wp-method test-suite generation, DOT
  serialize/parse/roundtrip, and end-to-end learning experiments.
  Run with `stack bench haal`.

## 0.4.1.0 - 2026-03-21

### Added 
- Serializer from Mealy Automaton to Dot format.
- haal-gen executable that accepts a .dot file and produces a haskell module
  that exports a function for the specified Mealy Automaton, with specific
  input and output types, rather than just using String for both.
- haal-models subpackage that exports learned models of tls, mqtt, tcp and dtls
  protocols.

## 0.4.0.2 - 2026-03-17

### Changed 
- Dependency bounds.

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
