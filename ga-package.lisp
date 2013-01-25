;; A genetic algorithm engine.
;;
;; Copyright Patrick May (patrick@softwarematters.org)

(in-package :common-lisp-user)

(defpackage :org.softwarematters.ga
  (:use :common-lisp)
  (:export #:bit-vector->integer
           #:integer->bit-vector
           #:bit-vector->gray-code
           #:gray-code->bit-vector
           #:integer->gray-code
           #:gray-code->integer
           #:make-genome
           #:mutate-genome
           #:single-crossover
           #:segment-crossover
           #:make-gene-pool
           #:most-fit-genome
           #:tournament-select
           #:evolve-gene-pool
           #:genome-length
           #:fitness
           #:fitness-comparator
           #:lesser-comparator
           #:greater-comparator
           #:average-fitness
           #:terminator
           #:generation-terminator
           #:fitness-terminator
           #:solve))
