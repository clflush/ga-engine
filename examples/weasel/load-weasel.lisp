(load (compile-file "../../ga-package.lisp"))
(load (compile-file "../../ga.lisp"))
(load (compile-file "ga-weasel-package.lisp"))
(load (compile-file "ga-weasel.lisp"))

(in-package :org.softwarematters.ga.weasel)

;; initialize the random seed
(setf *random-state* (make-random-state t))

(solve-weasel (make-weasel-problem "Methinks it is like a weasel") 1000 0.02)

;; (solve-weasel (make-weasel-problem "Regardless, while we may have different preferred techniques for measuring information, we agree that the *ev* genome does in fact gain information.") 1000 0.02)
;;
;; ID is not a mechanistic theory, and it is not ID's task to match your pathetic level of detail in telling mechanistic stories.
;;
;; "Is this because of descent from a common ancestor, or because only these pathways (and their variations) can sustain life?  Evolutionists think the former is correct, cdesign proponentsists accept the latter view."
