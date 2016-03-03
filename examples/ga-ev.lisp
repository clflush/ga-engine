;;;; An implementation of Schneider's ev simulation.
;;;; Copyright Patrick May (patrick@softwarematters.org)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(in-package :org.softwarematters.ga.ev)

;; Memoizing utility from Paul Graham's "On Lisp", page 65.
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
            val
            (setf (gethash args cache) (apply fn args)))))))

(defclass ev-problem ()
  ((number-of-possible-sites :initarg :number-of-possible-sites
                             :reader number-of-possible-sites)
   (number-of-binding-sites :initarg :number-of-binding-sites
                            :reader number-of-binding-sites)
   (binding-site-width :initarg :binding-site-width
                       :reader binding-site-width)
   (bases-per-integer :initarg :bases-per-integer
                      :reader bases-per-integer)
   (overlap-allowed-p :initarg :overlap-allowed-p
                      :reader overlap-allowed-p
                      :initform nil)
   (genome-length :accessor genome-length)
   (threshold-start :accessor threshold-start)
   (binding-sites-start :accessor binding-sites-start)
   (binding-sites :accessor binding-sites))
  (:documentation "The configuration for an ev run."))

(defun make-ev-problem (number-of-possible-binding-sites
                        number-of-binding-sites
                        binding-site-width
                        bases-per-integer
                        &key (overlap-allowed-p nil))
  (make-instance 'ev-problem
                 :number-of-possible-sites number-of-possible-binding-sites
                 :number-of-binding-sites number-of-binding-sites
                 :binding-site-width binding-site-width
                 :bases-per-integer bases-per-integer
                 :overlap-allowed-p overlap-allowed-p))

(defmethod generate-binding-sites ((problem ev-problem))
  (let ((binding-sites (list)))
    (dotimes (i (number-of-binding-sites problem) binding-sites)
      (if (overlap-allowed-p problem)
          (push (random (number-of-possible-sites problem)) binding-sites)
          (push (do ((site (random (number-of-possible-sites problem))
                           (random (number-of-possible-sites problem))))
                    ((not (find-if (lambda (x)
                                     (< (abs (- site x))
                                        (binding-site-width problem)))
                                   binding-sites))
                     site))
                binding-sites)))))

;; Calculate derived values.
(defmethod initialize-instance :after ((problem ev-problem) &rest rest)
  (declare (ignore rest))
  (let ((recognizer-bases
         (* 4 (binding-site-width problem) (bases-per-integer problem)))
        (binding-bases (+ (number-of-possible-sites problem)
                          (1- (binding-site-width problem)))))
    (setf (genome-length problem)
          (* 2 binding-bases))
    (setf (threshold-start problem)
          (* 2 recognizer-bases))
    (setf (binding-sites-start problem) 0)
    (setf (binding-sites problem) (generate-binding-sites problem))))

;; Version with separate recognizer section
;; (defmethod initialize-instance :after ((problem ev-problem) &rest rest)
;;   (declare (ignore rest))
;;   (let ((recognizer-bases
;;          (* 4 (binding-site-width problem) (bases-per-integer problem)))
;;         (threshold-bases (bases-per-integer problem))
;;         (binding-bases (+ (number-of-possible-sites problem)
;;                           (1- (binding-site-width problem)))))
;;     (setf (genome-length problem)
;;           (* 2 (+ recognizer-bases threshold-bases binding-bases)))
;;     (setf (threshold-start problem)
;;           (* 2 recognizer-bases))
;;     (setf (binding-sites-start problem)
;;           (* 2 (+ recognizer-bases threshold-bases)))
;;     (setf (binding-sites problem) (generate-binding-sites problem))))

(defclass ev-recognizer ()
  ((weights :initarg :weights
            :reader weights))
  (:documentation "A recognizer for an ev configuration and genome."))

;; Create a recognizer from the genome based on the problem parameters.
(defmethod make-ev-recognizer ((problem ev-problem) genome)
  (let* ((binding-site-width (binding-site-width problem))
         (weights (make-array (list binding-site-width 4)
                              :element-type 'integer
                              :initial-element 0))
         (bits-per-integer (* 2 (bases-per-integer problem))))
    (dotimes (i binding-site-width)
      (dotimes (j 4)
        (let* ((start (* (+ (* i 4) j) bits-per-integer))
               (end (+ start bits-per-integer)))
          (setf (aref weights i j)
                (bit-vector->twos-complement (subseq genome start end))))))
    (make-instance 'ev-recognizer :weights weights)))

;; Return the threshold value from the genome based on the problem parameters.
(defmethod threshold ((problem ev-problem) genome)
  (let* ((bits-per-integer (* 2 (bases-per-integer problem)))
         (start (threshold-start problem))
         (end (+ start bits-per-integer)))
    (bit-vector->twos-complement (subseq genome start end))))

;; Apply an ev-recognizer to a potential binding site.
(defmethod recognize ((problem ev-problem)
                      (recognizer ev-recognizer)
                      genome
                      index)
  (let ((sites-start (binding-sites-start problem))
        (total 0))
    (dotimes (i (binding-site-width problem)
              (>= total (threshold problem genome)))
      (let* ((start (+ sites-start (* 2 (+ index i))))
             (end (+ start 2))
             (base-value (bit-vector->integer (subseq genome start end))))
        (incf total (aref (weights recognizer) i base-value))))))

;; The fitness of a particular genome.  Required by ga-engine.
(defmethod fitness ((problem ev-problem) genome)
  (let ((recognizer (make-ev-recognizer problem genome))
        (matched 0)
        (missed 0)
        (spurious 0))
    (dotimes (i (number-of-possible-sites problem)
              (values (- 0 missed spurious) matched missed spurious))
      (let ((recognized (recognize problem recognizer genome i))
            (binding-site-p (not (null (position i (binding-sites problem))))))
        (and recognized binding-site-p (incf matched))
        (and recognized (not binding-site-p) (incf spurious))
        (and (not recognized) binding-site-p (incf missed))))))

;; Calculate r frequency for the specified problem.
(defmethod r-frequency ((problem ev-problem))
  (- (log (/ (number-of-binding-sites problem)
             (number-of-possible-sites problem)) 2)))

;; Using defparameter instead of defconstant due to SBCL idiosyncracy.
(defparameter +small-sampling-adjustment+
  (list 0.00000
        0.00000 0.75000 1.11090 1.32399 1.46291
        1.55923 1.62900 1.68129 1.72155 1.75328
        1.77879 1.79966 1.81699 1.83159 1.84403
        1.85475 1.86408 1.87227 1.87952 1.88598
        1.89177 1.89699 1.90172 1.90604 1.90998
        1.91361 1.91695 1.92003 1.92290 1.92557
        1.92805 1.93038 1.93256 1.93460 1.93652
        1.93834 1.94005 1.94167 1.94321 1.94466
        1.94604 1.94736 1.94861 1.94980 1.95094
        1.95203 1.95307 1.95407 1.95503 1.95594))

;; Calculate the sampling adjustment for small numbers of sites.
(defmethod calculate-sampling-adjustment((problem ev-problem))
  (- 2.0 (/ 3 (* 2 (log 2) (number-of-binding-sites problem)))))

;; Determine the sampling adjustment for small numbers of sites.
(defmethod sampling-adjustment((problem ev-problem))
  (let ((number-of-sites (number-of-binding-sites problem)))
    (if (< number-of-sites (length +small-sampling-adjustment+))
        (nth number-of-sites +small-sampling-adjustment+)
        (calculate-sampling-adjustment problem))))

;; Calculate Rsequence for the gene given the ev problem parameters.
;; The algorithm is:
;;   for each binding site position
;;     count the number of each base at that position across all sites
;;     calculate the uncertainty of that position
;;     accumulate the uncertainty into R sequence
(defmethod r-sequence ((problem ev-problem) genome)
  (flet ((base-counts (index)
           (let ((base-counts (make-array 4
                                          :element-type 'integer
                                          :initial-element 0))
                 (sites-start (binding-sites-start problem)))
             (dolist (binding-site (binding-sites problem) base-counts)
               (let* ((start (+ sites-start (* 2 (+ binding-site index))))
                      (end (+ start 2))
                      (base (bit-vector->integer (subseq genome start end))))
                 (incf (aref base-counts base))))))
         (position-uncertainty (base-counts)
           (let ((uncertainty 0.0)
                 (binding-site-count (number-of-binding-sites problem)))
             (dotimes (i (length base-counts) uncertainty)
               (when (> (aref base-counts i) 0)
                 (let ((frequency (/ (aref base-counts i) binding-site-count)))
                   (incf uncertainty (- (* frequency (log frequency 2))))))))))
    (let ((r 0.0))
      (dotimes (i (binding-site-width problem) r)
        (incf r (- (sampling-adjustment problem)
                   (position-uncertainty (base-counts i))))))))

(defmethod fitness-comparator ((problem ev-problem))
  "Return a fitness comparator function that takes two genomes and
  returns T if the first is more fit according to the characteristics of
  the PROBLEM."
  (memoize (greater-comparator problem)))
;  (greater-comparator problem))

(defun ev-interim-result-writer (gene-pool generation)
  "Write interim results including Rsequence."
  (let ((most-fit-genome (most-fit-genome gene-pool)))
    (multiple-value-bind (fitness matched missed spurious)
        (fitness (problem gene-pool) most-fit-genome)
      (format t "~&Generation:  ~D, best fitness = ~A (~A/~A/~A), Rsequence = ~A~%"
              generation
              fitness
              matched
              missed
              spurious
              (r-sequence (problem gene-pool) most-fit-genome)))))

