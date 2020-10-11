;;;; genetic.lisp

(in-package #:genetic)

;;;; A simple genetic algortihm problem
;;;; It uses genetic algorithms to generate a string of bits
;;;; That when converted to decimal represent the number 42

;;;; bits is a list of 6 1's and 0's.
;;;; Ex. '(1 0 0 0 0 0) is 32

;; A chromosome is a list of 6 bits representing a decimal number.

(defparameter *number-of-bits* 6 "The number of bits in a chromosome")

(defclass chromosome ()
  ((bits :initarg :bits
	 :accessor bits)))

(defmethod chromosome->decimal ((c chromosome))
  (apply #'+ (mapcar #'* (bits c) '(32 16 8 4 2 1))))

(defmethod fitness ((c chromosome))
  "Fitness is defined as the absolute distance from 42." 
  (abs (- 42 (chromosome->decimal c))))

(defmethod crossover ((a chromosome) (b chromosome))
  "Returns a new chromosome by splicing two together at a random point."
  (flet ((splice (a b crossover-point)
	   (append (take crossover-point a)
		   (drop crossover-point b))))
    (splice-chromosomes a-chromosome b-chromosome (random *number-of-bits*))))

(defun random-bits (n)
  "Returns a list of n random bits."
  (let ((result '()))
    (dotimes (i n result)
      (push (random 2) result))))

(defun create-random-chromosome ()
  "Creates a random chromosome." 
  (make-instance 'chromosome :bits (random-bits 6)))
