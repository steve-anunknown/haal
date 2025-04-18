```text


         _       _           _                   _                   _     
        / /\    / /\        / /\                / /\                _\ \   
       / / /   / / /       / /  \              / /  \              /\__ \  
      / /_/   / / /       / / /\ \            / / /\ \            / /_ \_\ 
     / /\ \__/ / /       / / /\ \ \          / / /\ \ \          / / /\/_/ 
    / /\ \___\/ /       / / /  \ \ \        / / /  \ \ \        / / /      
   / / /\/___/ /       / / /___/ /\ \      / / /___/ /\ \      / / /       
  / / /   / / /       / / /_____/ /\ \    / / /_____/ /\ \    / / / ____   
 / / /   / / /       / /_________/\ \ \  / /_________/\ \ \  / /_/_/ ___/\ 
/ / /   / / /       / / /_       __\ \_\/ / /_       __\ \_\/_______/\__\/ 
\/_/    \/_/        \_\___\     /____/_/\_\___\     /____/_/\_______\/     
                                                                           

```
## A Haskell library for Active Automata Learning

**Haal** is an [Active Automata Learning](https://wcventure.github.io/Active-Automata-Learning/) library aimed at making it easy to construct learning experiments and explore different configurations of learning algorithms and equivalence oracles. The library is still in its early stages, so nothing is set in stone yet. A summary of the current architecture can be found below — but of course, the best documentation is the source code itself.

## Architecture

The library consists of the following main components:
- Systems under learning
- Automata
- Learning algorithms
- Equivalence oracles
- Experiment

### Systems under learning

Systems under learning are defined by the `SUL` typeclass, parameterized by the types of their inputs and outputs. They must implement functions that allow them to be queried by learning algorithms and equivalence oracles.

### Automata

Automata are a subclass of systems under learning that also expose information about their internal states. In addition to being queryable, they must implement functions that expose useful structural information, particularly for use by equivalence oracles.

### Learning algorithms

Learning algorithms are used to construct hypotheses based on a `SUL`. They must support initialization, hypothesis construction, and refinement via counterexamples. Not all learning algorithms learn the same type of automaton; the type of automaton to be learned is determined by the algorithm itself. At the moment, all learning algorithms are represented by a typeclass and each specific learning algorithm is meant to be its own data type. The values of the data type are the different configurations of the algorithm.

### Equivalence oracles

Equivalence oracles are algorithms that generate a test suite of queries, which are sent to both the current hypothesis and the `SUL`, in order to discover counterexamples. They must implement functions to report the size of the test suite, generate the suite, and test the hypothesis for counterexamples. As in the case of learning algorithm, equivalence oracles as a whole are represented by a typeclass and a specific equivalence oracle algorithm is meant to be represented by a data type, with the possible values of it being the different configurations of the algorithm.

### Experiment

An experiment ties all the components above together. In this library, experiments are represented as values and can be built with various configurations. An experiment is a function that takes a learning algorithm and an oracle, and returns an environment that awaits a `SUL` to execute the experiment. This is implemented using a `Reader` monad, where the environment provides access to the `SUL`.

## Installing

The project is still in its early stages and has not yet been published. For now, you can clone the repository, build, and install it locally using:

```bash
stack install
```

## Example

Here's a quick GHCi session putting it all together, showing how to define a simple Mealy machine, configure an experiment, and run a learning algorithm using `haal`.

```haskell
ghci> :set +m
ghci> import qualified Data.Set as Set

-- Define input, output, and state types
ghci> data Input = A | B deriving (Show, Eq, Ord, Enum, Bounded)
ghci> data Output = X | Y deriving (Show, Eq, Ord, Enum, Bounded)
ghci> data State = S0 | S1 | S2 deriving (Show, Eq, Ord, Enum, Bounded)

-- Define the transition function for the system under learning
ghci> let sulTransitions S0 _ = (S1, X)
ghci|     sulTransitions S1 _ = (S2, Y)
ghci|     sulTransitions S2 A = (S0, X)
ghci|     sulTransitions S2 B = (S0, Y)

-- Set up the experiment. A bit clunky at the moment.
ghci> myexperiment = experiment (Lstar (error "don't worry" :: ObservationTable Input Output)) (WMethod 2)

-- Define the Mealy system under learning. Remember that automata can act as suls.
ghci> mysul = mkMealyAutomaton2 sulTransitions (Set.fromList [S0, S1, S2]) S0

-- Run the experiment
ghci> learnedmodel = runExperiment myexperiment mysul

-- View the learned model
ghci> learnedmodel
{
    Current State: 0,
    Initial State: 0,
    Transitions: fromList [
        ((0,A),(1,X)),
        ((0,B),(1,X)),
        ((1,A),(2,Y)),
        ((1,B),(2,Y)),
        ((2,A),(0,X)),
        ((2,B),(0,Y))
    ]
}
```

This shows how a simple Mealy machine can be learned using the `L*` algorithm and a `W`-method equivalence oracle.

This also showcases some strong points of using a functional programming language like haskell for the task of active automata learning:

### 1. Type-safe alphabets

In Haal, the input and output alphabets are represented as plain Haskell data types. This means the compiler can catch errors early — for example, if a symbol not defined in the alphabet accidentally appears in a transition, the type checker will reject the code. This eliminates entire classes of bugs that are easy to make in more loosely typed implementations.

```haskell
data Input = A | B deriving (Show, Eq, Ord, Enum, Bounded)
data Output = X | Y deriving (Show, Eq, Ord, Enum, Bounded)
```

### 2. Automata as functions

Instead of defining transitions via tables or external formats like `dot`, Haskell allows transitions to be encoded as pure functions:

```haskell
sulTransitions :: State -> Input -> (State, Output)
sulTransitions S0 _ = (S1, X)
sulTransitions S1 _ = (S2, Y)
sulTransitions S2 A = (S0, X)
sulTransitions S2 B = (S0, Y)
```

Combined with exhaustive pattern matching and totality checking, this ensures that:
- All input cases are handled for each state
- No states or transitions are forgotten
- The definition is both human-readable and machine-checkable

In essence, **Haskell itself is the language for defining automata**, without needing external DSLs or formats like DOT.

---


