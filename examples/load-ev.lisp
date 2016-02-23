(load (compile-file "../ga-package.lisp"))
(load (compile-file "../ga.lisp"))
(load (compile-file "ga-ev-package.lisp"))
(load (compile-file "ga-ev.lisp"))

(in-package :org.softwarematters.ga.ev)

;; initialize the random seed
(setf *random-state* (make-random-state t))

(defparameter *default-ev-problem*
  (make-ev-problem 256 16 6 5))

(let* ((problem *default-ev-problem*)
       (gene-pool (solve problem 64 (generation-terminator 1500)
                         :selection-method :truncation-selection
                         :mutation-count 1
                         :interim-result-writer #'ev-interim-result-writer))
       (best-genome (most-fit-genome gene-pool (fitness-comparator problem))))
  (format t "~%Best = ~F~%Average = ~F~%~%"
          (fitness problem best-genome)
          (average-fitness problem gene-pool)))

; TODO:  try other selection models
;        try crossover again
;        try with fitness including number correct (?)
;        change crossover to use-crossover
;        add :MUTATE-PARENTS
;        replace dolists with mapcar in r-sequence



