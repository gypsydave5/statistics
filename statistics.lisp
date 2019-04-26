(defpackage :com.gypsydave5.statistics
  (:use #:common-lisp)
  (:export #:mean
           #:mode
           #:median
           #:variance
           #:standard-deviation
           #:standard-score
           #:range
           #:interquartile-range))

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
                       ((> count max)
                        (setf max count)
                        (setf modes (list value))))) counts)
    (values modes max)))

(defun range (ns)
  "Calculates the range of NS"
  (let ((max (apply #'max ns))
        (min (apply #'min ns)))
    (- max min)))

(defun quantiles (q p)
  "Given a number of quantiles Q for a population P, returns a list of the
 members of P at each quantile.i.e. for quartiles Q should be 4, and the
 result will be (Q1 Q2 Q3)"
  (let ((sorted-p (sort p #'<)))
    (loop for k from 1 to (1- q)
       collect (quantile k q sorted-p))))

(defun quantile (k q sorted-ns)
  (let ((x (* k (/ (length sorted-ns) q))))
    (if (integerp x)
        (mean (list (nth (1- x) sorted-ns)
                    (nth x sorted-ns)))
        (nth (floor x) sorted-ns))))

(defun quartiles (ns)
  (quantiles 4 ns))

(defun interquantile-range (q qm qn xs)
  (let ((qs (quantiles q xs)))
    (values (- (nth (1- qn) qs)
               (nth (1- qm) qs))
            qs)))

(defun interquartile-range (xs)
  (interquantile-range 4 1 3 xs))

(defun square (x)
  (* x x))

(defun variance (xs)
  (let ((µ (mean xs)))
    (- (/ (apply #'+ (mapcar #'square xs))
          (length xs))
       (square μ))))

(defun standard-deviation (xs)
  (sqrt (variance xs)))

(defun standard-score (x xs)
  (let ((µ (mean xs))
        (σ (standard-deviation xs)))
    (/ (- x μ) σ)))



