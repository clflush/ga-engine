(ql:quickload :lparallel)
(setf lparallel:*kernel* (lparallel:make-kernel 8))

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
  (declare (ignore problem))
  (lambda (generation gene-pool)
    (let ((fitness (best-fitness (fitness-comparator (problem gene-pool))
                                 (fitnesses gene-pool))))
      (when (or (>= generation generations)
                (>= fitness 0))
        (format t "Generation = ~A, fitness = ~A~%" generation fitness)
        t))))


(dotimes (i 10)
  (let* ((problem *default-ev-problem*)
         (gene-pool (solve problem 500 (ev-terminator problem 10000)
                           :selection-method :tournament-selection
                           :mutation-rate 0.01
                           :mutate-parents nil
                           :use-crossover nil
                           :interim-result-writer nil))
         (best-genome (most-fit-genome gene-pool)))
    (format t "Best = ~F (~F)~%Average = ~F~%~%"
            (fitness problem best-genome)
            (r-sequence problem best-genome)
            (average-fitness gene-pool))))

#|
Before gene-pool changes:

Evaluation took:
  709.576 seconds of real time
  689.443870 seconds of total run time (686.533049 user, 2.910821 system)
  [ Run times consist of 7.683 seconds GC time, and 681.761 seconds non-GC time. ]
  97.16% CPU
  11 lambdas converted
  1,976,986,769,394 processor cycles
  79,145,458,592 bytes consed

After gene-pool changes:

Evaluation took:
  92.245 seconds of real time
  69.045109 seconds of total run time (68.334610 user, 0.710499 system)
  [ Run times consist of 0.808 seconds GC time, and 68.238 seconds non-GC time. ]
  74.85% CPU
  60 lambdas converted
  257,009,255,223 processor cycles
  3 page faults
  7,768,214,240 bytes consed
|#

#|
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


