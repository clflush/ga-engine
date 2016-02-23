;;;; An implementation of Schneider's ev simulation.
;;;; Copyright Patrick May (patrick@softwarematters.org)

(in-package :common-lisp-user)

(defpackage :org.softwarematters.ga.ev
  (:nicknames :ga.ev :ga-ev)
  (:use :common-lisp
        :org.softwarematters.ga)
  (:export #:make-ev-problem
           #:genome-length
           #:threshold-start
           #:binding-sites-start
           #:fitness-comparator
           #:fitness))
