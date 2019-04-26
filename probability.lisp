(defpackage #:probability
  (:use #:cl)
  (:export #:bayes))

(in-package #:probability)

(defun bayes (pA p!A pA->B p!A->B)
  (let* ((all-ways-to-A-and-B (* pA pA->B))
         (all-ways-to-!A-and-B (* p!A p!A->B))
         (all-ways-to-B (+ all-ways-to-A-and-B all-ways-to-!A-and-B)))
    (/ all-ways-to-A-and-B all-ways-to-B)))

(defparameter *slots* (list :dollar 1/10 :cherry 2/10 :lemon 2/10 :other 5/10))

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  (if (null bag) '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations (remove e bag :count 1 :test #'eq))))
              bag)))

(defun permutations (xs result)
  (cond ((null xs) (list xs))
        ((null result) (permutations xs (mapcar #'list xs)))
        ((= (length xs) (length (first result))) result)
        (t (permutations xs
                         (mapcan #'(lambda (l) (mapcar #'(lambda (x) (cons x l))
                                                       (set-exclusive-or l xs)))
                                 result)))))

(mapcan #'(lambda (l) (mapcar #'(lambda (x) (cons x l))
                              (set-exclusive-or l (list 1 2 3))))
        '((1) (2) (3)))
;; => ((3 1) (2 1) (3 2) (1 2) (2 3) (1 3))

(set-exclusive-or (list 1 2 3) (list 2 3 4))
;; => (4 1)

(defvar x (list 1 2 3 4 5 6))

(permutations '(1 2 3) nil)

(defun probability (&rest triples)
  (apply #'+
         (mapcar #'(lambda (triple)
                     (apply #'* (mapcar #'(lambda (s)
                                            (getf *slots* s))
                                        triple)))
                 triples)))
;; probability of three dollars
(probability '(:dollar :dollar :dollar))
;; => 1/1000

;; probability of dollar-dollar-cherry (any order)
(probability '(:dollar :dollar :cherry)
             '(:dollar :cherry :dollar)
             '(:cherry :dollar :dollar))
;; => 3/500

;; probability of lemon-lemon-lemon
(probability '(:lemon :lemon :lemon))
;; => 1/125

;; probability of cherry-cherry-cherry
(probability '(:cherry :cherry :cherry))
;; => 1/125

;; probability of winning nothing
(- 1
   (probability '(:cherry :cherry :cherry))
   (probability '(:lemon :lemon :lemon))
   (probability '(:dollar :dollar :dollar))
   (probability '(:dollar :dollar :cherry)
                '(:dollar :cherry :dollar)
                '(:cherry :dollar :dollar)))
;; => 977/1000

(defparameter *dist* '(:nothing (977/1000 -1)
                       :lemons (1/125 4)
                       :cherries (1/125 9)
                       :$cherries (3/500 14)
                       :dollars (1/1000 19)))

(defun plist-values (plist)
  (loop for v in (rest plist) by #'cddr
     collect v))

(defun expectation (ps)
  (apply #'+ (mapcar #'(lambda (p) (apply #'* p)) ps)))

(expectation (plist-values *dist*))
;; => -77/100

(set-difference '(1 2 2 2 3 ) '(2 7))
;; => (3 1)

(defun last-word (word)
  (let ((cs (coerce word 'list)))
    (coerce (reduce #'(lambda (word c)
                        (if (char> (first word) c)
                            (append word (list c))
                            (cons c word)))
                    (rest cs)
                    :initial-value (list (first cs)))
            'string)))

(last-word "CAR")

(mapcar #'last-word (list "CAB"
                          "JAM"
                          "CODE"
                          "ABAAB"
                          "CABCBBABC"
                          "ABCABCABC"
                          "ZXCASDQWE"))
;; => ("CAB" "MJA" "OCDE" "BBAAA" "CCCABBBAB" "CCCBAABAB" "ZXCASDQWE")

(apply #'* (list 1 2 3 4 5 ))
;; => 120


(defun scramble (source target)
          (let ((source (coerce source 'list))
                (target (coerce target 'list)))
            (cond ((null target) t) 
                  ((member (first target) source)
                   (scramble (remove (first target) source :count 1)
                             (rest target))))))

(mapcar #'(lambda (args) (apply #'scramble args))
        '(("rkqodlw" "world")
          ("cedewaraaossoqqyt" "codewars")
          ("katas" "steak")
          ("scriptjavx" "javascript")
          ("scriptingjava" "javascript")
          ("scriptsjava" "javascripts")
          ("javscripts" "javascript")
          ("aabbcamaomsccdd" "commas")
          ("commas" "commas")
          ("sammoc" "commas")))
;; => (T T NIL NIL T T NIL T T T)

(defun guess-blue (blue-in red-in blue-out red-out)
  (let* ((blue (- blue-in blue-out))
         (red (- red-in red-out))
         (total (+ red blue)))
    (/ blue total)))

(guess-blue 5 5 2 3)
;; => 3/5

(guess-blue 1 2 0 0)
;; => 1/3