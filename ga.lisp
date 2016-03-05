;; A genetic algorithm engine.
;;
;; Copyright Patrick May (patrick@softwarematters.org)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(in-package :org.softwarematters.ga)

;; Utility functions.
(defmacro while (test &body body)
  "A little syntactic sugar around DO."
  `(do () ((not ,test)) ,@body))

(defun bit-vector->integer (bits)
  "Create a positive integer from a bit vector."
  (reduce (lambda (first-bit second-bit) (+ (* first-bit 2) second-bit)) bits))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (integer &optional (accumulator nil))
             (cond ((> integer 0) (integer->bit-list (truncate integer 2)
                                                     (push (mod integer 2)
                                                           accumulator)))
                   ((null accumulator) (push 0 accumulator))
                   (t accumulator))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun bit-vector->gray-code (bits)
  "Convert from standard binary to binary reflected Gray code."
  (let ((gray-bits (make-array (length bits)
                               :element-type 'bit
                               :initial-element 0)))
    (dotimes (i (length bits) gray-bits)
      (if (zerop i)
          (setf (bit gray-bits i) (bit bits i)) ; big-endian
          (setf (bit gray-bits i) (logxor (bit bits (1- i)) (bit bits i)))))))

(defun gray-code->bit-vector (gray-bits)
  "Convert from binary reflected Gray code to standard binary."
  (let ((bits (make-array (length gray-bits)
                          :element-type 'bit
                          :initial-element 0)))
    (dotimes (i (length gray-bits) bits)
      (if (zerop i)
          (setf (bit bits i) (bit gray-bits i)) ; big-endian
          (setf (bit bits i) (logxor (bit bits (1- i)) (bit gray-bits i)))))))

(defun gray-code->integer (bits)
  "Use the utilities provided by the GA package to construct a function
  that computes an integer value from a bit-vector in Gray code."
  (bit-vector->integer (gray-code->bit-vector bits)))

(defun integer->gray-code (integer)
  "Create a Gray code bit-vector from a positive integer."
  (bit-vector->gray-code (integer->bit-vector integer)))

(defun bit-vector->twos-complement (bits)
  "Convert a bit vector into an integer using two's complement rules."
  (if (= 0 (bit bits 0))
      (bit-vector->integer bits)
      (- (1+ (bit-vector->integer (bit-not bits))))))

;;; Problem methods.

(defgeneric genome-length (problem)
  (:documentation
   "Returns the number of bits required to represent a candidate
   solution to the PROBLEM."))

(defgeneric fitness (problem genome)
  (:documentation
   "Return the fitness of the bit string GENOME in the context of the
   PROBLEM.  This value is only meaningful in that context."))

(defgeneric fitness-comparator (problem)
  (:documentation
   "Return a fitness comparator function that takes two fitnesses and
   returns T if the first is more fit according to the characteristics
   of the PROBLEM."))

(defun lesser-comparator (problem)
  "Return a fitness comparator function that takes two fitnesses and
  returns T if the first has a lower value for its fitness."
  (declare (ignore problem))
  #'<)

(defun greater-comparator (problem)
  "Return a fitness comparator function that takes two fitnesses and
  returns T if the first has a higher value for its fitness."
  (declare (ignore problem))
  #'>)

;; The genetic algorithm functions.

(defun make-genome (length &optional (distribution 0.5))
  "Create a randomized bit array of the specified LENGTH.  A value
  between 0 and 1 can be specified for DISTRIBUTION to modify the mean
  relative number of 1 bits."
  (let ((genome (make-array length :element-type 'bit :initial-element 0)))
    (map-into genome (lambda () (if (> distribution (random 1.0)) 1 0)))))

(defun single-crossover (parent-one parent-two)
  "Create two new genomes by crossing over PARENT-ONE and PARENT-TWO at
  a single, randomly selected point."
  (let ((crossover-point (random (length parent-one))))
    (list (concatenate 'bit-vector
                       (subseq parent-one 0 crossover-point)
                       (subseq parent-two crossover-point))
          (concatenate 'bit-vector
                       (subseq parent-two 0 crossover-point)
                       (subseq parent-one crossover-point)))))

(defun segment-crossover (parent-one parent-two &optional (segments 1))
  "Create two new genomes by crossing over SEGMENTS segments of
  PARENT-ONE and PARENT-TWO."
  (let ((child-one (copy-seq parent-one))
        (child-two (copy-seq parent-two)))
    (dotimes (i segments (list child-one child-two))
      (let* ((start (random (length child-one)))
             (end (+ start (random (- (length child-one) start))))
             (segment-one (copy-seq (subseq child-one start end)))
             (segment-two (copy-seq (subseq child-two start end))))
        (setf (subseq child-one start end) segment-two)
        (setf (subseq child-two start end) segment-one)))))

(defclass gene-pool ()
  ((problem :initarg :problem
            :reader problem)
   (size :initarg :size
         :reader size)
   (initial-distribution :initarg :initial-distribution
                         :reader initial-distribution
                         :initform 0.5)
   (genomes :accessor genomes)
   (fitnesses :accessor fitnesses)
   (generation :accessor generation
               :initform 0))
  (:documentation "A pool of genomes."))

;; (defun make-gene-pool (size genome-length &optional (distribution 0.5))
;;   "Create a list of SIZE genomes, each of length GENOME-LENGTH."
;;   (let ((pool nil))
;;     (dotimes (count size pool)
;;       (push (make-genome genome-length distribution) pool))))

(defun make-gene-pool (problem size &optional (distribution 0.5))
  "Create an instance of a gene-pool."
  (make-instance 'gene-pool
                 :problem problem
                 :size size
                 :initial-distribution distribution))

(defmethod initialize-instance :after ((gene-pool gene-pool) &rest args)
  (declare (ignore args))
  (let ((pool nil))
    (dotimes (count (size gene-pool))
      (push (make-genome (genome-length (problem gene-pool))
                         (initial-distribution gene-pool))
            pool))
    (setf (genomes gene-pool) pool)))

(defmethod calculate-fitness ((gene-pool gene-pool))
  "Calculate the fitnesses for all genomes in the GENE-POOL."
  (setf (fitnesses gene-pool)
        (mapcar (lambda (genome)
                  (fitness (problem gene-pool) genome))
                (genomes gene-pool))))

(defun best-fitness (fitness-comparator fitnesses)
  "Return the best fitness value in the list of FITNESSES based on the
  FITNESS-COMPARATOR."
  (reduce (lambda (fitness-one fitness-two)
            (if (funcall fitness-comparator fitness-one fitness-two)
                fitness-one
                fitness-two))
          fitnesses))

(defmethod most-fit-genome ((gene-pool gene-pool))
  "Return the most fit genome in GENE-POOL."
  (let* ((best-fitness (best-fitness (fitness-comparator (problem gene-pool))
                                     (fitnesses gene-pool)))
         (index (position best-fitness (fitnesses gene-pool))))
    (elt (genomes gene-pool) index)))

(defmethod average-fitness ((gene-pool gene-pool))
  "Return the average fitness of the GENE-POOL."
  (/ (apply #'+ (fitnesses gene-pool)) (size gene-pool)))

(defparameter *tournament-select-percentage* 0.2)

(defun tournament-select (gene-pool
                          &key (select-percent *tournament-select-percentage*))
  "Randomly select a pool of genomes that are TOURNAMENT-SELECT-PERCENTAGE
  of the full GENE-POOL and apply FITNESS-COMPARATOR to return the best one.
  FITNESS-COMPARATOR must take two genomes as arguments and return T if the
  first is the the most fit of the two."
  (let* ((pool-size (size gene-pool))
         (tournament-size (floor (* pool-size select-percent)))
         (tournament nil)
         (fitnesses nil)
         (indices nil))
    (while (< (length tournament) tournament-size)
      (let ((index (random pool-size)))
        (unless (find index indices)
          (push (elt (genomes gene-pool) index) tournament)
          (push (elt (fitnesses gene-pool) index) fitnesses)
          (push index indices))))
    (let* ((best-fitness
            (best-fitness (fitness-comparator (problem gene-pool)) fitnesses))
           (best-index (position best-fitness fitnesses)))
      (elt tournament best-index))))

(defun tournament-evolve (gene-pool mutation-operator
                          &key (select-percent *tournament-select-percentage*)
                               (use-crossover nil)
                               (mutate-parents nil))
  "Evolve a new gene pool using tournament selection."
  (let ((size (size gene-pool))
        (new-pool nil))
    (while (< (length new-pool) size)
      (let* ((parent-one (tournament-select gene-pool
                                            :select-percent select-percent))
             (parent-two (tournament-select gene-pool
                                            :select-percent select-percent))
             (child-one (funcall mutation-operator (copy-seq parent-one)))
             (child-two (funcall mutation-operator (copy-seq parent-two))))
        (when mutate-parents
          (push (funcall mutation-operator parent-one) new-pool)
          (push (funcall mutation-operator parent-two) new-pool))
        (unless mutate-parents
          (push parent-one new-pool)
          (push parent-two new-pool))
        (when use-crossover
          (let ((children (single-crossover child-one child-two)))
            (push (car children) new-pool)
            (push (cadr children) new-pool)))
        (unless use-crossover
          (push child-one new-pool)
          (push child-two new-pool))))
    (setf (genomes gene-pool) new-pool)))
                          
(defun roulette-select (gene-pool best-fitness)
  "Randomly select a genomes from the GENE-POOL returning the first where
  the fitness of the genome divided by the BEST-FITNESS is greater
  than a randomly generated percentage."
  (let ((pool-size (size gene-pool)))
    (do ((genome (elt (genomes gene-pool) (random pool-size))
                 (elt (genomes gene-pool) (random pool-size))))
        ((>= (/ best-fitness (fitness (problem gene-pool) genome))
             (random 1.0))
         genome))))
; TODO:  fix above to handle different fitness comparators

(defun roulette-evolve (gene-pool mutation-operator
                        &key (use-crossover nil)
                             (mutate-parents nil))
  "Evolve a new gene pool using roulette wheel selection."
  (let ((size (size gene-pool))
        (best-fitness (best-fitness (fitness-comparator (problem gene-pool))
                                    (fitnesses gene-pool)))
        (new-pool nil))
    (while (< (length new-pool) size)
      (let* ((parent-one (roulette-select gene-pool best-fitness))
             (parent-two (roulette-select gene-pool best-fitness))
             (child-one (funcall mutation-operator (copy-seq parent-one)))
             (child-two (funcall mutation-operator (copy-seq parent-two))))
        (when mutate-parents
          (push (funcall mutation-operator parent-one) new-pool)
          (push (funcall mutation-operator parent-two) new-pool))
        (unless mutate-parents
          (push parent-one new-pool)
          (push parent-two new-pool))
        (when use-crossover
          (let ((children (single-crossover child-one child-two)))
            (push (car children) new-pool)
            (push (cadr children) new-pool)))
        (unless use-crossover
          (push child-one new-pool)
          (push child-two new-pool))))
    (setf (genomes gene-pool) new-pool)))

(defun all-positions (element sequence)
  "Return a list of all indices where ELEMENT occurs in SEQUENCE."
  (let ((positions nil))
    (dotimes (i (length sequence) (nreverse positions))
      (when (eq (elt sequence i) element)
        (push i positions)))))

(defparameter *truncate-select-percentage* 0.5)

(defun truncate-select (gene-pool
                        &key (select-percent *truncate-select-percentage*))
  "Select the top SELECT-PERCENT percentage of the GENE-POOL and return
  them in order of fitness."
  (let* ((comparator (fitness-comparator (problem gene-pool)))
         (ordered-fitnesses
          (remove-duplicates
           (sort (copy-seq (fitnesses gene-pool))
                 (lambda (fitness-one fitness-two)
                   (funcall comparator fitness-one fitness-two)))))
         (selected-size (floor (* (size gene-pool) select-percent)))
         (selected nil))
    (while (< (length selected) selected-size)
      (let ((indices
             (all-positions (pop ordered-fitnesses) (fitnesses gene-pool))))
        (dolist (index indices)
          (when (< (length selected) selected-size)
            (push (elt (genomes gene-pool) index) selected)))))
    (nreverse selected)))


;; (let* ((comparator (greater-comparator 'foo))
;;        (fitnesses (list 0 1 2 3 4 5 5 4 3 2 1 0 0))
;;        (sorted (remove-duplicates (sort (copy-seq fitnesses)
;;                                         (lambda (x y)
;;                                           (funcall comparator x y))))))
;;   (all-positions (pop sorted) fitnesses))
;; (5 6)



  ;; (let ((comparator (fitness-comparator (problem gene-pool))))
  ;;   (nreverse (nthcdr (floor (* (size gene-pool) select-percent))
  ;;                     (sort (copy-seq (genomes gene-pool))
  ;;                           (lambda (x y)
  ;;                             (funcall comparator y x)))))))

(defun truncate-evolve (gene-pool mutation-operator
                        &key (select-percent *truncate-select-percentage*)
                             (mutate-parents t))
  "Evolve a new gene pool using truncation selection."
  (let* ((size (size gene-pool))
         (selected (truncate-select gene-pool
                                    :select-percent select-percent))
         (new-pool (mapcar (lambda (genome)
                             (if mutate-parents
                                 (mutate-n-bits (copy-seq genome) 1)
                                 genome))
                           selected)))
    (while (< (length new-pool) size)
      (dolist (genome selected)
        (when (< (length new-pool) size)
          (push (funcall mutation-operator (copy-seq genome)) new-pool))))
    (setf (genomes gene-pool) new-pool)))

;; Solution generators

(defgeneric terminator (generation gene-pool)
  (:documentation
   "Return T if the run should terminate based on the current GENERATION
   and GENE-POOL."))

(defun generation-terminator (generations)
  "Return a termination function that stops processing after GENERATIONS
  generations."
  (lambda (generation gene-pool)
    (declare (ignore gene-pool))
    (>= generation generations)))

(defun fitness-terminator (fitness)
  "Return a termination function that stops processing when the best
  solution in the gene pool has fitness greater than or equal to FITNESS
  in the context of the PROBLEM."
  (lambda (generation gene-pool)
    (declare (ignore generation))
    (>= (best-fitness (fitness-comparator (problem gene-pool))
                      (fitnesses gene-pool))
        fitness)))

(defun default-interim-results (gene-pool generation)
  (format t "~&Generation:  ~D, best fitness = ~A~%"
          generation
          (best-fitness (fitness-comparator (problem gene-pool))
                        (fitnesses gene-pool))))

(defun mutate-genome (genome rate)
  "Flip bits in the GENOME bit-vector with a percentage chance equal to
  the specified RATE (ranging from 0 to 1)."
  (bit-xor genome (make-genome (length genome) rate)))

(defun mutate-n-bits (genome n)
  "Mutate exactly N bits in the GENOME."
  (dotimes (i n genome)
    (let ((index (random (length genome))))
      (if (= 0 (aref genome index))
          (setf (aref genome index) 1)
          (setf (aref genome index) 0)))))

(defun solve (problem pool-size terminator
              &key (genome-bit-distribution 0.5)
                   (interim-result-writer #'default-interim-results)
                   (selection-method :tournament-selection)
                   mutation-rate
                   mutation-count
                   use-crossover
                   mutate-parents
                   (tournament-select-percentage *tournament-select-percentage*)
                   (truncate-select-percentage *truncate-select-percentage*))
  "Evolve a solution to PROBLEM using a gene pool of POOL-SIZE until
  TERMINATOR returns true.  Return the final gene pool."
  (flet ((make-mutation-operator ()
           (cond ((and mutation-rate (not mutation-count))
                  (lambda (genome)
                    (mutate-genome genome mutation-rate)))
                 ((and mutation-count (not mutation-rate))
                  (lambda (genome)
                    (mutate-n-bits genome mutation-count)))
                 (t (error "Set exactly one of mutation rate or count."))))
         (make-tournament-evolver (mutation-operator)
           (lambda (gene-pool)
             (tournament-evolve gene-pool mutation-operator
                                :use-crossover use-crossover
                                :mutate-parents mutate-parents
                                :select-percent tournament-select-percentage)))
         (make-roulette-evolver (mutation-operator)
           (lambda (gene-pool)
             (roulette-evolve gene-pool mutation-operator
                              :use-crossover use-crossover
                              :mutate-parents mutate-parents)))
         (make-truncation-evolver (mutation-operator)
           (lambda (gene-pool)
             (truncate-evolve gene-pool mutation-operator
                              :mutate-parents mutate-parents
                              :select-percent truncate-select-percentage))))
    (let* ((mutation-operator (make-mutation-operator))
           (evolve-gene-pool
            (case selection-method
              (:tournament-selection
               (make-tournament-evolver mutation-operator))
              (:roulette-selection (make-roulette-evolver mutation-operator))
              (:truncation-selection
               (make-truncation-evolver mutation-operator))))
           (gene-pool (make-gene-pool problem
                                      pool-size
                                      genome-bit-distribution))
           (generation 0))
      (calculate-fitness gene-pool)
      (while (not (funcall terminator generation gene-pool))
        (when interim-result-writer
          (funcall interim-result-writer gene-pool generation))
        (funcall evolve-gene-pool gene-pool)
        (calculate-fitness gene-pool)
        (incf generation))
      gene-pool)))

  ;; change to (evolve-gene-pool problem gene-pool gene-pool-fitness)
