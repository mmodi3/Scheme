#lang eopl
;; This lab will make you code Chinese Remainder Theorem. It has split CRT into several small parts
;; to make it more doable. EuclidAlgo (used to compute gcd) and egcd (also known as pulverizer)
;; (used to compute the linear combination of the gcd) have been provided for you.

;; tuplelist contains (a b) pairs that stand for a (mod b)

;; Given 2 integers this returns the gcd
(define (EuclidAlgo int1 int2)
  (if (zero? int2)
      int1
      (EuclidAlgo int2 (modulo int1 int2))))

;; Given 2 integers this returns the list (gcd(a,b) x y) that represents the expression gcd(a,b) = xa + yb
(define (egcd a b)
  (if (zero? a)
      (list b 0 1)
      (list (car (egcd (modulo b a) a))
            (- (caddr (egcd (modulo b a) a)) (* (quotient b a) (cadr (egcd (modulo b a) a))))
            (cadr (egcd (modulo b a) a)))))

;; Define mul_inv
;; This should compute the multiplicative inverse of a
;; This means a*x (mod b) === 1 and x is the multiplicative inverse of a
;; To do this you should use egcd since the egcd computes x when given relatively prime numbers

;; Examples:
;; (mul_inv 31 76)-> 27
;; (mul_inv 127 555) -> 118
;; (mul_inv 1234 4321) -> -1082

;; Type Signature: (mul_inv int int) -> int

(define (mul_inv a b)
  (car (cdr (egcd a b))))

;; Define M
;; M should compute the product of all numbers inside the mod (b)
;; This should be a simple multiplication recursion on the second number in each pair

;; Examples:
;; (M '((2 3) (3 5) (2 7))) -> 105
;; (M '((10 11) (4 12) (12 13))) -> 1716
;; (M '((1 5) (2 14) (5 23) (26 27))) -> 43470

;; Type Signature: (M list-of-(a,b)-tuples) -> int

(define (mh tuplelist prod)
  (if (null? tuplelist)
      prod
      (mh (cdr tuplelist) (* (car (cdr (car tuplelist))) prod))))

(define (M tuplelist)
  (mh tuplelist 1))




;; Define CRT-exists?
;; This should return a boolean that represents if the CRT is possible
;; CRT is possible iff all numbers inside the mod (b) are pairwise relatively prime
;; To check this check every combinattion of 2 b's
;; This can be done by comparing the first b in the list with every other b in the list
;; and if the gcd of any pair isn't one return false and if they are all one
;; recurse on the list without the first element and check again until the list is empty
;; A helper may be useful.

;; Examples:
;; (CRT-exists? '((2 3) (3 5) (2 6))) -> #f
;; (CRT-exists? '((10 11) (4 12) (12 13))) -> #t
;; (CRT-exists? '((1 5) (2 14) (5 23) (26 28))) -> #f
;; (CRT-exists? '((1 2) (1 3) (1 5) (1 7) (1 11) (1 13))) -> #t

;; Type Signature: (CRT-exists? list-of-(a,b)-tuples) -> boolean


  

(define (CRT-exists? tuplelist)
  (if (null? tuplelist)
       #t
       (if (= 1 (EuclidAlgo (car (cdr (car tuplelist)))  (M (cdr tuplelist))))
           (CRT-exists? (cdr tuplelist))
           #f)))
           



;; Define CRT-helper
;; CRT-helper is given a valid tuplelist and M (product of all b in the list) and
;; computes what the value of the CRT is without simplifying
;; To do this you need to add Ai*Mi*Mi^-1 for all i in the list and sum these values
;; Ai is the first element of each tuple, Mi is m/b where b is the second element of each tuple
;; and m is the product of all b in the list, and Mi^-1 is the mul_inv of Mi mod Bi
;; Examples:
;; (CRT-helper '((10 11) (4 12) (12 13)) 1716) -> -17876
;; (CRT-helper '((2 3) (3 5) (2 7)) 105) -> 23
;; (CRT-helper '((1 2) (2 3) (3 5) (4 7) (5 11) (6 13)) 30030) -> -30817

;; Type Signature: (CRT-helper list-of-(a,b)-tuples int) -> int

(define (CRT-helper tuplelist m)
  (if (null? tuplelist)
      0
      (+ (* (* (car (car tuplelist)) (/ m (car (cdr (car tuplelist))))) (mul_inv (/ m (car (cdr (car tuplelist)))) (car (cdr (car tuplelist))))) (CRT-helper (cdr tuplelist) m))))
     
;; Define CRT
;; This should put together all of the previous functions to make a fully working CRT calculator
;; You should first check if the CRT is possible to be computed
;; If it is possible to be computed you should return the smallest positive integer that fulfills
;;the requirements by using CRT-helper and M. If it isn't possible you should return -1.

;; Examples:
;; (CRT '((10 11) (4 12) (12 13))) -> 1000
;; (CRT '((2 3) (3 5) (2 7))) -> 23
;; (CRT '((1 2) (2 3) (3 5) (4 7) (5 11) (6 13))) -> 29243
;; (CRT '((1 2) (4 8) (8 9))) -> -1

;; Type Signature: (CRT list-of-(a,b)-tuples int) -> int

(define (CRT tuplelist)
    (if (CRT-exists? tuplelist)
        (modulo (CRT-helper tuplelist (M tuplelist)) (M tuplelist))
        -1))
        
