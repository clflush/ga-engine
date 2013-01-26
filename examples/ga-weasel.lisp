;; An alternative implementation of Richard Dawkins' Weasel Program.
;; Copyright Patrick May (patrick@softwarematters.org)
(in-package :org.softwarematters.ga.weasel)

(defun string->bit-vector (string)
  "Create a bit vector from a STRING, representing each character in
  7 bit ASCII."
  (let ((bit-vector (make-array (* 7 (length string))
                                :element-type 'bit
                                :initial-element 0)))
    (dotimes (i (length string) bit-vector)
      (let* ((char-bits (integer->bit-vector (char-code (char string i))))
             (offset (+ (* i 7) (- 7 (length char-bits))))
             (end (+ offset (length char-bits))))
        (setf (subseq bit-vector offset end) char-bits)))))

(defun bit-vector->string (bit-vector)
  "Create a string from a BIT-VECTOR, representing each character in
  7 bit ASCII."
  (let* ((string-length (/ (length bit-vector) 7))
         (string (make-string string-length)))
    (dotimes (i string-length string)
      (let* ((offset (* i 7))
             (end (+ offset 7))
             (value (bit-vector->integer (subseq bit-vector offset end))))
        (setf (aref string i) (character value))))))

(defclass weasel-problem ()
  ((target-string :initarg :target-string
                  :reader target-string
                  :initform nil)
   (target-genome :accessor target-genome :initform nil))
  (:documentation "The configuration for a Steiner network problem."))

(defmethod initialize-instance :after ((problem weasel-problem) &rest rest)
  "Compute the target genome for the WEASEL-PROBLEM."
  (declare (ignore rest))
  (setf (target-genome problem) (string->bit-vector (target-string problem))))

(defun make-weasel-problem (target-string)
  "Create an instance of WEASEL-PROBLEM with the specified TARGET-STRING."
  (make-instance 'weasel-problem :target-string target-string))

(defmethod genome-length ((problem weasel-problem))
  "Determine the number of bits required for a genome to solve the
  specified Weasel problem."
  (length (target-genome problem)))

(defmethod fitness ((problem weasel-problem) genome)
  "The fitness function for a Weasel problem is the number of bits
  that match between the genome and the target genome."
  (count 1 (bit-eqv genome (target-genome problem))))

(defmethod fitness-comparator ((problem weasel-problem))
  "Return a fitness comparator function that takes two genomes and
  returns T if the first is more fit according to the characteristics of
  the PROBLEM."
  (greater-comparator problem))

(defun solve-weasel (problem population-size mutation-rate)
  "Run the GA engine against the PROBLEM until the fitness is maximize."
  (let* ((gene-pool
          (solve problem
                 population-size
                 mutation-rate
                 (fitness-terminator problem
                                     (length (target-genome problem)))))
         (best-genome (most-fit-genome gene-pool
                                       (fitness-comparator problem))))
    (format t "~%Best = ~F~%Average = ~F~%"
            (fitness problem best-genome)
            (average-fitness problem gene-pool))))

(solve-weasel (make-weasel-problem "Methinks it is like a weasel") 1000 0.02)

