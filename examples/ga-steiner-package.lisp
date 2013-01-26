;; A solution to Steiner networks using a genetic algorithm engine.
;; Copyright Patrick May (patrick@softwarematters.org)
(in-package :common-lisp-user)

(defpackage :org.softwarematters.ga.steiner
  (:use :common-lisp
        :org.softwarematters.ga)
  (:export #:make-steiner-problem
           #:fixed-nodes
           #:genome-length
           #:variable-node-coords
           #:variable-nodes
           #:nodes
           #:node-connections
           #:connections
           #:fitness-comparator
           #:fitness))
