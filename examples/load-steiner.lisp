(load (compile-file "../ga-package.lisp"))
(load (compile-file "../ga.lisp"))
(load (compile-file "ga-steiner-package.lisp"))
(load (compile-file "ga-steiner.lisp"))

(in-package :org.softwarematters.ga.steiner)

;; initialize the random seed
(setf *random-state* (make-random-state t))

;(defparameter *five-point-problem*
;  (make-steiner-problem '((150 0) (450 0) (0 260) (600 260) (300 433)) 7))
;
;(solve-steiner *five-point-problem* 1000 1000 0.02)

(defparameter *panda-problem*
  (make-steiner-problem
   '((0 0) (400 0) (800 0) (0 300) (400 300) (800 300)) 9))

(solve-steiner *panda-problem* 1000 500 0.01)

