;; A genetic algorithm engine.
;;
;; Copyright Patrick May (patrick@softwarematters.org)

(in-package :common-lisp-user)

(defpackage :org.softwarematters.ga
  (:nicknames :ga)
  (:use :common-lisp :lparallel)
  (:export #:bit-vector->integer
           #:integer->bit-vector
           #:bit-vector->gray-code
           #:gray-code->bit-vector
           #:integer->gray-code
           #:gray-code->integer
           #:bit-vector->twos-complement
           #:fitness
           #:fitness-comparator
           #:lesser-comparator
           #:greater-comparator
           #:make-genome
           #:problem
           #:size
           #:genomes
           #:fitnesses
           #:best-fitness
           #:average-fitness
           #:mutate-genome
           #:single-crossover
           #:segment-crossover
           #:make-gene-pool
           #:most-fit-genome
           #:tournament-select
           #:evolve-gene-pool
           #:genome-length
           #:terminator
           #:generation-terminator
           #:fitness-terminator
           #:tournament-selection
           #:roulette-selection
           #:truncation-selection
           #:solve))
