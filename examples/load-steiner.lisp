(load (compile-file "/Users/Patrick/src/lisp/ga/ga-package.lisp"))
(load (compile-file "/Users/Patrick/src/lisp/ga/ga.lisp"))
(load (compile-file "/Users/Patrick/src/lisp/ga/ga-steiner-package.lisp"))
(load (compile-file "/Users/Patrick/src/lisp/ga/ga-steiner.lisp"))
;(load "/Users/Patrick/src/lisp/ga/ga-steiner-gui-package.lisp")
;(load "/Users/Patrick/src/lisp/ga/ga-steiner-gui.lisp")

(in-package :org.softwarematters.ga.steiner)

;; initialize the random seed
(setf *random-state* (make-random-state t))

(defparameter *five-point-problem*
  (make-steiner-problem '((150 0) (450 0) (0 260) (600 260) (300 433)) 7))

(solve-steiner *five-point-problem* 1000 500 0.02)

