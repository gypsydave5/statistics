(ql:quickload "lisp-unit")
(load "statistics")

(defpackage :com.gypsydave5.statistics-test
  (:use #:common-lisp #:lisp-unit #:com.gypsydave5.statistics))

(in-package :com.gypsydave5.statistics-test)

(defparameter *odd-length-numbers*
  (list 3 3 6 7 7 10 10 10 11 13 30))

(defparameter *even-length-numbers*
  (list 1 2 3 4 5 6))

(define-test mean-test
  (loop for (expected population)
     in `((7/2 ,*even-length-numbers*)
          (10 ,*odd-length-numbers*)
          (4 (3 4 5)))
     do (assert-equal expected (mean population) population)))

(define-test median-test
  (assert-equal 7/2 (median *even-length-numbers*))
  (assert-equal 10 (median *odd-length-numbers*))
  (assert-equal 3 (median (list 5 3 2 4 1)))
  (assert-equal 3.0 (median (list 1.0 2.0 4.0 5.0))))

(define-test mode-test
  (assert-equality #'set-equal *even-length-numbers* (mode *even-length-numbers*))
  (assert-equal (list 10) (mode *odd-length-numbers*))
  (assert-equality #'set-equal (list 1 3) (mode (list 1 3 1 3 1 3 1 2 3 4))))

(define-test variance-test
  (assert-equal 38/3 (variance (list 1 2 9))))

(define-test standard-deviation-test
  (assert-equal (sqrt 38/3) (standard-deviation (list 1 2 9))))

(defun run-statistics-tests ()
  (let ((lisp-unit:*print-errors* t)
        (lisp-unit:*print-failures* t))
    (run-tests)))

(run-statistics-tests)
