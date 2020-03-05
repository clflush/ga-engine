;; A solution to Steiner networks using a genetic algorithm engine.
;; Copyright Patrick May (patrick@softwarematters.org)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(in-package :org.softwarematters.ga.steiner)

;; Memoizing utility from Paul Graham's "On Lisp", page 65.
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
            val
            (setf (gethash args cache) (apply fn args)))))))

;; Steiner problem genome:
;; bits                             | description
;; 0-xbits                          | x coordinate of variable node 0
;; xbits-ybits                      | y coordinate of variable node 0
;; . . .
;; (* (+ xbits ybits) n)            | start of x coordinate of variable node n
;; (+ (* (+ xbits ybits) n) xbits)  | start of y coordinate of variable node n
;; . . .
;; remainder                        | connection mask

(defclass steiner-problem ()
  ((fixed-nodes :initarg :fixed-nodes
                :reader fixed-nodes
                :initform (make-array 0 :element-type 'list :fill-pointer t))
   (variable-node-count :initarg :variable-node-count
                        :accessor variable-node-count
                        :initform 0)
   (x-dimension :initarg :x-dimension :reader x-dimension :initform 1024)
   (y-dimension :initarg :y-dimension :reader y-dimension :initform 1024)
   (x-coord-bits :accessor x-coord-bits :initform 10)
   (y-coord-bits :accessor y-coord-bits :initform 10))
  (:documentation "The configuration for a Steiner network problem."))

(defmethod initialize-instance :after ((problem steiner-problem) &rest rest)
  "Compute all derived values."
  (declare (ignore rest))
  (setf (x-coord-bits problem) (ceiling (log (x-dimension problem) 2)))
  (setf (y-coord-bits problem) (ceiling (log (y-dimension problem) 2))))

(defun make-steiner-problem (fixed-nodes max-variable-nodes)
  "Create an instance of STEINER-PROBLEM with the fixed nodes at the
  coordinates specified by FIXED-NODES and a maximum of
  MAX-VARIABLE-NODES variable nodes."
  (let ((max-x (apply #'max (mapcar #'car fixed-nodes)))
        (max-y (apply #'max (mapcar #'cadr fixed-nodes))))
    (make-instance 'steiner-problem
                   :fixed-nodes (make-array (length fixed-nodes)
                                            :element-type 'list
                                            :fill-pointer t
                                            :initial-contents fixed-nodes)
                   :variable-node-count max-variable-nodes
                   :x-dimension max-x
                   :y-dimension max-y)))

;; TODO: eliminate need for x/y-coords to be 2^n - 1.

(defmethod total-node-count ((problem steiner-problem))
  "Return the maximum number of nodes possible in the specified PROBLEM."
  (+ (length (fixed-nodes problem)) (variable-node-count problem)))

(defmethod genome-length ((problem steiner-problem))
  "Determine the number of bits required for a genome to solve the
  specified Steiner network."
  (let ((total-nodes (total-node-count problem)))
    (+ (* (variable-node-count problem)
          (+ (x-coord-bits problem) (y-coord-bits problem)))
       (* total-nodes (/ (1- total-nodes) 2)))))

;; Use Peter Norvig's technique to replace the function associated with
;; the symbol gray-code->integer by a memoized version of itself.
(setf (symbol-function 'gray-code->integer)
      (memoize (symbol-function 'gray-code->integer)))

(setf (symbol-function 'bit-vector->integer)
      (memoize (symbol-function 'bit-vector->integer)))

(defmethod variable-node-coords ((problem steiner-problem) genome index)
  "Return a list containing the x and y coordinates contained in the
  GENOME of the variable node identified by INDEX."
  (let* ((x-bits (x-coord-bits problem))
         (y-bits (y-coord-bits problem))
         (x-start (* index (+ x-bits y-bits)))
         (y-start (+ x-start x-bits)))
    (list (gray-code->integer (subseq genome x-start y-start))
          (gray-code->integer (subseq genome y-start (+ y-start y-bits))))))

(defmethod variable-nodes ((problem steiner-problem) genome)
  "Return a list of the locations of the variable nodes in the
  specified GENOME based on the characteristics of the particular
  PROBLEM."
  (let ((nodes nil))
    (dotimes (i (variable-node-count problem) nodes)
      (push (variable-node-coords problem genome i) nodes))))

(defmethod nodes ((problem steiner-problem) genome)
  "Return a vector of the locations of the fixed and variable nodes in
  the specified GENOME based on the characteristics of the particular
  PROBLEM."
  (concatenate 'vector (fixed-nodes problem) (variable-nodes problem genome)))

(defmethod connection-mask ((problem steiner-problem) genome index)
  "Return the bit-vector containing the connections associated with the
  node identified by INDEX in the specified GENOME."
  (flet ((summation (limit) (* (/ limit 2) (1+ limit))))
    (let ((start (+ (* (variable-node-count problem)
                       (+ (x-coord-bits problem) (y-coord-bits problem)))
                    (- (* index (total-node-count problem))
                       (summation index))))
          (mask-length (1- (- (total-node-count problem) index))))
      (subseq genome start (+ start mask-length)))))

(defmethod node-connections ((problem steiner-problem) genome index)
  "Return a list containing the other nodes to which the node identified
  by INDEX is connected in the specified GENOME based on the
  characteristics of the particular PROBLEM."
  (let ((connection-mask (connection-mask problem genome index))
        (connections nil))
    (dotimes (i (length connection-mask) connections)
      (unless (zerop (bit connection-mask i))
        (push (+ index i 1) connections)))))

(defmethod connections ((problem steiner-problem) genome)
  "Return a list of all unique pairs of connected nodes in the specified
  GENOME."
  (let ((connections nil))
    (dotimes (i (total-node-count problem) connections)
      (dolist (node (node-connections problem genome i))
        (push (list i node) connections)))))

(defmethod distance ((problem steiner-problem) genome node-one node-two)
  "Compute the distance between NODE-ONE and NODE-TWO within the
  specified GENOME constrained by the PROBLEM."
  (let* ((nodes (nodes problem genome))
         (coord-one (aref nodes node-one))
         (coord-two (aref nodes node-two))
         (x-diff (abs (- (car coord-one) (car coord-two))))
         (y-diff (abs (- (cadr coord-one) (cadr coord-two)))))
    (declare (fixnum x-diff y-diff))
    (sqrt (+ (* x-diff x-diff) (* y-diff y-diff)))))

(defun connected-nodes (node-list connections)
  "Identify all nodes reachable from the nodes in NODE-LIST based on the
  specified CONNECTIONS."
  (let ((associated-connections
         (remove-if (lambda (connection)
                      (null (intersection connection node-list)))
                    connections)))
    (if (null associated-connections)
        (values node-list connections)
        (connected-nodes (remove-duplicates
                          (union node-list
                                 (reduce #'append associated-connections)))
                         (set-difference connections
                                         associated-connections
                                         :test 'equal)))))

(defmethod connected ((problem steiner-problem) genome)
  "Determine whether or not the GENOME specifies a fully connected graph
  given the constraints of the PROBLEM."
  (labels ((connected-nodes (node-list connections)
             (let ((associated-connections
                    (remove-if (lambda (connection)
                                 (null (intersection connection node-list)))
                               connections)))
               (if (null associated-connections)
                   (values node-list connections)
                   (connected-nodes (remove-duplicates
                                     (union node-list
                                            (reduce #'append
                                                    associated-connections)))
                                    (set-difference connections
                                                    associated-connections
                                                    :test 'equal)))))
           (fixed-node-ids (problem)
             (let ((ids nil))
               (dotimes (i (length (fixed-nodes problem)) ids)
                 (push i ids)))))
    (let ((connections (connections problem genome)))
      (multiple-value-bind (connected-nodes other-connections)
          (connected-nodes (car connections) (cdr connections))
        (declare (ignore other-connections))
        (null (set-difference (fixed-node-ids problem) connected-nodes))))))

(defconstant +unconnected-penalty+ 2000)

(defmethod fitness ((problem steiner-problem) genome)
  "The fitness function for a Steiner problem is the total length of all
  the paths between connected points, with a large penalty for solutions
  that do not connect all the nodes."
  (let ((total-length
         (reduce #'+ (mapcar (lambda (connection)
                               (distance problem genome
                                         (car connection) (cadr connection)))
                             (connections problem genome)))))
    (if (connected problem genome)
        total-length
        (+ total-length +unconnected-penalty+))))

(defmethod fitness-comparator ((problem steiner-problem))
  "Return a fitness comparator function that takes two fitnesses and
  returns T if the first is more fit according to the characteristics of
  the PROBLEM."
  (lesser-comparator problem))

(defun solve-steiner (problem population-size generations mutation-rate)
  "Run the GA engine against the PROBLEM for GENERATIONS generations and
  print the results."
  (let* ((gene-pool (solve problem
                           population-size
                           (generation-terminator generations)
                           :selection-method :tournament-selection
                           :mutation-rate mutation-rate
                           :mutate-parents t
                           :use-crossover t))
         (best-genome (most-fit-genome gene-pool)))
    (format t "~%Best = ~F~%Average = ~F~%Nodes = ~S~%Connections = ~S~%"
            (fitness problem best-genome)
            (average-fitness gene-pool)
            (nodes problem best-genome)
            (connections problem best-genome))))
