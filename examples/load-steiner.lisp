(load (compile-file "~/projects/ga-engine/ga-package.lisp"))
(load (compile-file "~/projects/ga-engine/ga.lisp"))
(load (compile-file "~/projects/ga-engine/examples/ga-steiner-package.lisp"))
(load (compile-file "~/projects/ga-engine/examples/ga-steiner.lisp"))

(in-package :org.softwarematters.ga.steiner)

;; initialize the random seed
(setf *random-state* (make-random-state t))

(defparameter *five-point-problem*
  (make-steiner-problem '((150 0) (450 0) (0 260) (600 260) (300 433)) 7))

;(solve-steiner *five-point-problem* 1000 1000 0.02)
;(solve-steiner *five-point-problem* 2000 250 0.02)

(defparameter *five-point-smaller-problem*
  (make-steiner-problem '((150 0) (450 0) (0 260) (600 260) (300 433)) 5))

(solve-steiner *five-point-smaller-problem* 1000 250 0.02)

(defparameter *panda-problem*
      (make-steiner-problem
       '((0 0) (400 0) (800 0) (0 300) (400 300) (800 300)) 9))

(defparameter *alt-panda-problem*
      (make-steiner-problem
       '((0 0) (400 0) (800 0) (0 300) (400 300) (800 300)) 15))

;(solve-steiner *panda-problem* 1000 500 0.01)
;(solve-steiner *panda-problem* 500 500 0.01)
;(solve-steiner *panda-problem* 1000 500 0.01)
;(solve-steiner *panda-problem* 1000 500 0.02)
;(solve-steiner *alt-panda-problem* 1000 500 0.01)


;; Best = 1212.6082
;; Average = 1588.5311
;; Nodes = #((150 0) (450 0) (0 260) (600 260) (300 433) (586 449) (148 239)
;;           (9 418) (81 359) (917 246) (212 79) (431 47))
;; Connections = ((10 11) (6 10) (4 6) (3 11) (2 6) (1 11) (0 10))

;; Best = 1212.6167
;; Average = 1558.6553
;; Nodes = #((150 0) (450 0) (0 260) (600 260) (300 433) (854 259) (300 260)
;;           (397 122) (150 173) (450 173) (278 371) (300 377))
;; Connections = ((6 8) (6 9) (6 11) (4 11) (3 9) (2 8) (1 9) (0 8))

