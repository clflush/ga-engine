(load (compile-file "../ga-package.lisp"))
(load (compile-file "../ga.lisp"))
(load (compile-file "ga-ev-package.lisp"))
(load (compile-file "ga-alt-ev.lisp"))

(in-package :org.softwarematters.ga.ev)

;; initialize the random seed
(setf *random-state* (make-random-state t))

(defparameter *default-ev-problem*
  (make-ev-problem 256 16 6 5))

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

(let* ((problem *default-ev-problem*)
       (gene-pool (solve problem 64 (generation-terminator 2000)
                         :selection-method :truncation-selection
                         :mutation-count 1
                         :mutate-parents t
                         :interim-result-writer #'ev-interim-result-writer))
       (best-genome (most-fit-genome gene-pool)))
  (format t "~%Best = ~F~%Average = ~F~%~%"
          (fitness problem best-genome)
          (average-fitness gene-pool)))
