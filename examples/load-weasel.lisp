(load (compile-file "../ga-package.lisp"))
(load (compile-file "../ga.lisp"))
(load (compile-file "ga-weasel-package.lisp"))
(load (compile-file "ga-weasel.lisp"))

(in-package :org.softwarematters.ga.weasel)

;; initialize the random seed
(setf *random-state* (make-random-state t))

(solve-weasel (make-weasel-problem "Methinks it is like a weasel") 1000 0.02)

