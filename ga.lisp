;; A genetic algorithm engine.
;;
;; Copyright Patrick May (patrick@softwarematters.org)

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

;; The genetic algorithm functions.

(defun make-genome (length &optional (distribution 0.5))
  "Create a randomized bit array of the specified LENGTH.  A value
  between 0 and 1 can be specified for DISTRIBUTION to modify the mean
  relative number of 1 bits."
  (let ((genome (make-array length :element-type 'bit :initial-element 0)))
    (map-into genome (lambda () (if (> distribution (random 1.0)) 1 0)))))

(defun mutate-genome (genome rate)
  "Flip bits in the GENOME bit-vector with a percentage chance equal to
  the specified RATE (ranging from 0 to 1)."
  (bit-xor genome (make-genome (length genome) rate)))

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

(defun make-gene-pool (size genome-length &optional (distribution 0.5))
  "Create a list of SIZE genomes, each of length GENOME-LENGTH."
  (let ((pool nil))
    (dotimes (count size pool)
      (push (make-genome genome-length distribution) pool))))

(defun most-fit-genome (gene-pool fitness-comparator)
  "Return the most fit genome in GENE-POOL based on FITNESS-COMPARATOR.
  FITNESS-COMPARATOR must take two genomes as arguments and return T if
  the first is the the most fit of the two."
  (reduce (lambda (genome-one genome-two)
            (if (funcall fitness-comparator genome-one genome-two)
                genome-one
                genome-two))
          gene-pool))

(defun tournament-select (gene-pool fitness-comparator
                          &key (tournament-size 2))
  "Randomly select TOURNAMENT-SIZE genomes from the GENE-POOL and apply
  FITNESS-COMPARATOR to return the best one.  FITNESS-COMPARATOR must
  take two genomes as arguments and return T if the first is the the
  most fit of the two."
  (let* ((pool-size (length gene-pool))
         (tournament nil))
    (while (< (length tournament) tournament-size)
      (pushnew (elt gene-pool (random pool-size)) tournament))
    (most-fit-genome tournament fitness-comparator)))

(defun evolve-gene-pool (gene-pool fitness-comparator mutation-rate)
  "Create a new gene pool of the same size as GENE-POOL by replacing
  half the population with mutated offspring of tournament selection
  winners selected by FITNESS-COMPARATOR.  The other half of the
  population consists of the parent genomes.  MUTATION-RATE must be
  between 0 and 1."
  (let ((size (length gene-pool))
        (new-pool nil))
    (dotimes (i (/ size 4) new-pool)
      (let* ((parent-one (tournament-select gene-pool fitness-comparator))
             (parent-two (tournament-select gene-pool fitness-comparator))
             (children (mapcar (lambda (genome)
                                 (mutate-genome genome mutation-rate))
                               (single-crossover parent-one parent-two))))
        (push (copy-seq parent-one) new-pool)
        (push (copy-seq parent-two) new-pool)
        (push (car children) new-pool)
        (push (cadr children) new-pool)))))

;; Solution generators

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
   "Return a fitness comparator function that takes two genomes and
   returns T if the first is more fit according to the characteristics
   of the PROBLEM."))

(defun lesser-comparator (problem)
  "Return a fitness comparator function that takes two genomes and
  returns T if the first has a lower value for its fitness."
  (lambda (genome-one genome-two)
    (< (fitness problem genome-one) (fitness problem genome-two))))

(defun greater-comparator (problem)
  "Return a fitness comparator function that takes two genomes and
  returns T if the first has a higher value for its fitness."
  (lambda (genome-one genome-two)
    (> (fitness problem genome-one) (fitness problem genome-two))))

(defun average-fitness (problem gene-pool)
  "Return the average fitness of the GENE-POOL, in the context of the PROBLEM."
  (/ (apply #'+ (mapcar (lambda (genome) (fitness problem genome)) gene-pool))
     (length gene-pool)))

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

(defun fitness-terminator (problem fitness)
  "Return a termination function that stops processing when the best
  solution in the gene pool has fitness greater than or equal to FITNESS
  in the context of the PROBLEM."
  (lambda (generation gene-pool)
    (declare (ignore generation))
    (>= (fitness problem
                 (most-fit-genome gene-pool (fitness-comparator problem)))
        fitness)))

(defun solve (problem pool-size mutation-rate terminator
              &key (genome-bit-distribution 0.5)
                   (write-interim-results t))
  "Evolve a solution to PROBLEM using a gene pool of POOL-SIZE until
  TERMINATOR returns true.  Return the final gene pool."
  (let ((gene-pool (make-gene-pool pool-size
                                   (genome-length problem)
                                   genome-bit-distribution))
        (comparator (fitness-comparator problem))
        (generation 0))
    (while (not (funcall terminator generation gene-pool))
      (when write-interim-results
        (format t "~&Generation:  ~D, best fitness = ~A~%"
                generation
                (fitness problem (most-fit-genome gene-pool comparator))))
      (setf gene-pool (evolve-gene-pool gene-pool comparator mutation-rate))
      (incf generation))
    gene-pool))
