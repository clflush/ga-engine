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
;;; To run with the original, default ev settings, printing results every
;;; generation:

(let* ((problem *default-ev-problem*)
       (gene-pool (solve problem 64 (generation-terminator 3000)
                         :selection-method :truncation-selection
                         :mutation-count 1
                         :mutate-parents t
                         :interim-result-writer #'ev-interim-result-writer))
       (best-genome (most-fit-genome gene-pool)))
  (format t "~%Best = ~F~%Average = ~F~%~%"
          (fitness problem best-genome)
          (average-fitness gene-pool)))

;;; To test with overlap, create a problem like this:

(defparameter *overlapping-ev-problem*
  (make-ev-problem 256 16 6 5 :allow-overlap-p t))

;;; and/or simply set the locations of the binding sites explicitly,
;;; e.g.:

(setf (binding-sites *overlapping-ev-problem*)
      (list 0 2 10 12 20 22 30 32 40 42 50 52 60 62 70 72))

;;; To run multiple times with a larger population size and tournament
;;; selection, stopping when the first maximally fit genome appears, with
;;; no interim reporting:

(dotimes (i 10)
  (setf *random-state* (make-random-state t))
  (let* ((problem *default-ev-problem*)
         (gene-pool (solve problem 256 (ev-terminator problem 10000)
                           :selection-method :tournament-selection
                           :mutation-rate 0.005
                           :mutate-parents nil
                           :use-crossover nil
                           :interim-result-writer nil))
         (best-genome (most-fit-genome gene-pool)))
    (format t "Best = ~F (Rseq = ~F)~%Average = ~F~%~%"
            (fitness problem best-genome)
            (r-sequence problem best-genome)
            (average-fitness gene-pool))))
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

;; (let* ((problem *default-ev-problem*)
;;        (gene-pool (solve problem 64 (generation-terminator 3000)
;;                          :selection-method :truncation-selection
;;                          :mutation-count 1
;;                          :mutate-parents t
;;                          :interim-result-writer #'ev-interim-result-writer))
;;        (best-genome (most-fit-genome gene-pool)))
;;   (format t "~%Best = ~F~%Average = ~F~%~%"
;;           (fitness problem best-genome)
;;           (average-fitness gene-pool)))

(defparameter *overlapping-ev-problem*
  (make-ev-problem 256 16 6 5 :allow-overlap-p t))

(setf (binding-sites *overlapping-ev-problem*)
      (list 0 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120))

(let* ((problem *overlapping-ev-problem*)
       (gene-pool (solve problem 64 (generation-terminator 3000)
                         :selection-method :truncation-selection
                         :mutation-count 1
                         :mutate-parents t
                         :interim-result-writer #'ev-interim-result-writer))
       (best-genome (most-fit-genome gene-pool)))
  (format t "~%Best = ~F~%Average = ~F~%~%"
          (fitness problem best-genome)
          (average-fitness gene-pool)))
