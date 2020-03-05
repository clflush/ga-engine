;; An alternative implementation of Richard Dawkins' Weasel Program.
;; Copyright Patrick May (patrick@softwarematters.org)
(in-package :common-lisp-user)

(defpackage :org.softwarematters.ga.weasel
  (:use :common-lisp
        :org.softwarematters.ga)
  (:export #:make-weasel-problem
           #:genome-length
           #:fitness
           #:fitness-comparator))
