# ga-engine
====

A genetic algorithm engine in Common Lisp.

## Features

The GA engine is quite simple, consisting at present of less than 200
lines of code.  The symbols exported from the
```org.softwarematters.ga``` package include utilities, generic
functions, and core GA capabilities:

* Utilities
  - ```bit-vector->integer```
    Convert a bit vector to an integer.
  - ```integer->bit-vector```
   Convert an integer to a bit vector.
  - ```bit-vector->gray-code```
    Convert a bit vector to an equivalent bit vector in
    [Gray code](http://mathworld.wolfram.com/GrayCode.html).
    Gray code, named after Frank Gray, is a bit string encoding
    with the characteristic that adjacent integers differ by a
    single bit.  This is often useful in evolutionary computation,
    for performance reasons.
  - ```gray-code->bit-vector```
    Convert a Gray code encoded bit vector to an equivalent base 2
    bit vector.
  - ```gray-code->integer```
    Convert a Gray code encoded bit vector to an integer.
  - ```integer->gray-code```
    Convert an integer to a Gray encoded bit vector.
* Generic Functions
  The first three of these generic functions must be implemented for
  any problem passed to the GA engine.
  - ```genome-length```
    Return the number of bits required to encode a candidate
    solution in the context of a particular problem.
  - ```fitness```
    Return the fitness of a genome in the context of a particular
    problem.
  - ```fitness-comparator```
    Return a function that takes two genomes and returns true if
    the first is more fit according to the characteristics of the
    problem.
  - ```lesser-comparator```
    A utility function that returns a ```fitness-comparator``` that
    considers lower fitness scores to represent greater fitness.
  - ```greater-comparator```
    A utility function that returns a ```fitness-comparator``` that
    considers higher fitness scores to represent greater fitness.
  - ```terminator```
    Return a function that indicates whether or not a particular
    run should terminate, based on the current generation and gene
    pool.
  - ```generation-terminator```
    A utility function that returns a ```terminator``` that
    ends a run after a certain number of generations.
  - ```fitness-terminator```
    A utility function that returns a ```terminator``` that
    ends a run after a certain number of generations.
* Core Capabilities
  - ```make-genome```
    Return a bit vector of the specified length.
  - ```make-gene-pool```
    Create a list of the specified number of bit vectors of the
    specified length.
  - ```most-fit-genome```
    Return the most fit genome in a gene pool.
  - ```average-fitness```
    Return the average fitness of a gene pool.
  - ```mutate-genome```
    Flip bits in the specified genome with the specified
    probability.
  - ```single-crossover```
    Create two children from two parent bit vectors by crossing
    the parents at a randomly selected point.
  - ```tournament-select```
    Randomly select the specified number of genomes (two by
    default) from the specified gene pool and apply the specified
    fitness comparator to return the best one.
  - ```evolve-gene-pool```
    Create a new gene pool of the same size as the specified gene
    pool by replacing half the population with mutated offspring
    of tournament selection winners.  The other half of the
    population consists of the parent genomes.
  - ```solve```
    Evolve a solution by repeatedly calling ```evolve-gene-pool``` on
    a gene pool of the specified size until the specified terminator
    returns true.  Return the final gene pool.

## Usage

Applying the GA engine to a problem consists of following a few simple
steps:

* Create a class to represent the characteristics of the problem
* Implement a method to create instances of the problem class
* Implement the required generic functions for the problem:
  - ```genome-length```
  - ```fitness```
  - ```fitness-comparator```
* Implement a ```terminator``` function
* Run ```solve```

The Steiner Problem and Dawkin's Weasel examples show how to use the
GA engine.

## Planned Enhancements

I've got a number of enhancements in the pipeline, including:

* Allowing the mutation rate to gradually decrease over time to
  minimize jitter.
* Supporting temporary increases in the mutation rate when plateaus
  are detected.
* Support for additional selection mechanisms, i.e.:
  - Weighted Roulette wheel
  - Elitism
  - Ranked (the least fit gets rank 1, the most fit rank N) with
    Roulette wheel selection
* Support for crossover at multiple points.
  (```segment-crossover``` is already included in the source.)
* A Web interface

Please contact me by [email](mailto:patrick@softwarematters.org) if
you have any questions, comments, or suggestions.
