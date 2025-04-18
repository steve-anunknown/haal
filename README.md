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
