(defpackage :com.gypsydave5.statistics
  (:use #:common-lisp)
  (:export #:mean #:mode #:median #:variance #:standard-deviation))

(in-package #:com.gypsydave5.statistics)

;; mean
(defun mean (ns)
  "Calculates the mean of numbers NS"
  (when (null ns) (error "Cannot have a mean of no values"))
  (/ (apply #'+ ns) (length ns)))

(defun median (ns)
  "Calculates the median of numbers NS and the index of the median"
  (let* ((ns (sort ns #'<))
         (l (length ns))
         (mid (/ l 2)))
    (cond
      ((null ns) (error "Cannot have a median value of no values"))
      ((evenp l)
       (mean (list (nth (1- mid) ns)
                   (nth mid ns))))
      (t (nth (floor mid) ns)))))

(median (list 5 1 3 2 4))
;; => 3
(median (list 1.0 2.0 4.0 5.0))
;; => 3.0

(defun mode (xs)
  "Calculates the modes of XS and returns them as well as the count."
  (let ((counts (make-hash-table :test 'equal))
        (max 0)
        (modes ()))
    (dolist (x xs)
      (multiple-value-bind (count present) (gethash x counts)
        (if present
            (setf (gethash x counts) (1+ count))
            (setf (gethash x counts) 1))))
    (maphash #'(lambda (value count)
                 (cond ((= count max) (push value modes))
                       ((> count max) (progn (setf max count)
                                             (setf modes (list value)))))) counts)
    (values modes max)))


;; => (3 1)
;;    4

(mode (list'a 'b 'c 'd 'e 'a))
;; => (A)
;;    2


(defun range (&rest ns)
  "Calculates the range of NS"
  (let ((max (apply #'max ns))
        (min (apply #'min ns)))
    (- max min)))

(range 1 3 10 8)
;; => 9

(defun quantiles (q ns)
  (let ((sorted-ns (sort ns #'<)))
    (loop for k from 1 to (1- q)
       collect (quantile k q sorted-ns))))

(defun quantile (k q sorted-ns)
  (let ((x (* k (/ (length sorted-ns) q))))
        (if (integerp x)
            (mean (list (nth (1- x) sorted-ns)
                        (nth x sorted-ns)))
            (nth (floor x) sorted-ns))))

(defun quartiles (ns)
  (quantiles 4 ns))

(defparameter *odd-length-numbers*
  (list 3 3 6 7 7 10 10 10 11 13 30))

(defparameter *even-length-numbers*
  (list 1 2 3 4 5 6))

(quartiles *even-length-numbers*)
;; => (2 7/2 5)

(quartiles *odd-length-numbers*)
;; => (6 10 11)

(defun interquartile-range (&rest ns)
  (destructuring-bind (q1 _ q3) (quartiles ns)
    (declare (ignore _))
    (- q3 q1)))

(apply #'interquartile-range *odd-length-numbers*)
;; => 5

(defun square (x)
  (* x x))

(defun variance (xs)
  (let ((µ (mean xs)))
    (/ (apply #'+ (mapcar #'(lambda (x) (square (- µ x))) xs))
       (length xs))))

(defun standard-deviation (xs)
  (sqrt (variance xs)))

(standard-deviation (list 1 2 9))
