;(ql:quickload :lparallel)
;(setf lparallel:*kernel* (lparallel:make-kernel 4))

(load (compile-file "../ga-package.lisp"))
(load (compile-file "../ga.lisp"))
(load (compile-file "ga-ev-package.lisp"))
(load (compile-file "ga-ev.lisp"))

(in-package :org.softwarematters.ga.ev)

;; initialize the random seed
(setf *random-state* (make-random-state t))

(defparameter *default-ev-problem*
  (make-ev-problem 256 16 6 5))

#|
(let* ((problem *default-ev-problem*)
       (gene-pool (solve problem 64 (generation-terminator 2000)
                         :selection-method :truncation-selection
                         :mutation-count 1
                         :mutate-parents t
                         :interim-result-writer #'ev-interim-result-writer))
       (best-genome (most-fit-genome gene-pool (fitness-comparator problem))))
  (format t "~%Best = ~F~%Average = ~F~%~%"
          (fitness problem best-genome)
          (average-fitness problem gene-pool)))
|#

(defun ev-terminator (problem generations)
  "Return a termination function that stops processing when the best
  solution in the gene pool has fitness 0 or the number of generations is
  greater than or equal to GENERATIONS."
  (lambda (generation gene-pool)
    (let ((fitness (fitness problem
                            (most-fit-genome gene-pool
                                             (fitness-comparator problem)))))
      (when (or (>= generation generations)
                (>= fitness 0))
        (format t "Generation = ~A, fitness = ~A~%" generation fitness)
        t))))

(dotimes (i 10)
  (setf *random-state* (make-random-state t))
  (let* ((problem *default-ev-problem*)
         (gene-pool (solve problem 64 (ev-terminator problem 10000)
                           :selection-method :tournament-selection
                           :mutation-rate 0.005
                           :mutate-parents t
                           :interim-result-writer #'ev-interim-result-writer))
         (best-genome (most-fit-genome gene-pool (fitness-comparator problem))))
    (format t "Best = ~F (Rseq = ~F)~%Average = ~F~%~%"
            (fitness problem best-genome)
            (r-sequence problem best-genome)
            (average-fitness problem gene-pool))))

#|
(let* ((problem *default-ev-problem*)
       (gene-pool (solve problem 250 (ev-terminator problem 2000)
                         :selection-method :tournament-selection
                         :mutation-rate 0.01
                         :use-crossover t
                         :interim-result-writer #'ev-interim-result-writer))
       (best-genome (most-fit-genome gene-pool (fitness-comparator problem))))
  (format t "~%Best = ~F (Rseq = ~F)~%Average = ~F~%~%"
          (fitness problem best-genome)
          (r-sequence problem best-genome)
          (average-fitness problem gene-pool)))

                 (fitness-terminator problem
                                     (length (target-genome problem)))
|#

; TODO:  try tournament and roulette selection models
;        try crossover again
;        try mutation-rate vs mutation-count
;        enable binding site overlap
;        try different population sizes
;        try different recognizer
;        fix MUTATE-PARENTS for all selection methods
;        replace dolists with mapcar in r-sequence
;        allow TOURNAMENT-SELECT-COUNT


