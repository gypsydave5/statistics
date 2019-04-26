(load "probability")
(ql:quickload "lisp-unit")

(defpackage #:probability-test
  (:use #:cl #:lisp-unit #:probability))

(in-package #:probability-test)

(define-test test-given
  (let ((pBlack 18/38)
        (pBlack->Even 10/18)
        (pRed->Even 8/18)
        (pRed 18/38))
    (assert-equal 5/9 (bayes pBlack pRed pBlack->Even pRed->Even))))

(defun run-probability-tests ()
  (let ((lisp-unit:*print-errors* t)
        (lisp-unit:*print-failures* t))
    (run-tests)))
