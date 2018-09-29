#lang eopl
;; modulo is the call for %
;; quotient is the call for integer division


;; Define EuclidAlgo
;; This algorithm is used to find the gcd of two numbers.
;; It works by using the fact that gcd(a,b) = gcd(b,(a%b))
;; Once b is zero a will be the gcd of the two numbers.

;; Examples:
;; (EuclidAlgo 37 1234) -> 1
;; (EuclidAlgo 1532732 180) -> 4
;; (EuclidAlgo 1234 43210) -> 2

;; Type signature: (EuclidAlgo int int) -> int

(define (EuclidAlgo int1 int2)
      (if (zero? int2)
          int1
          (EuclidAlgo int2 (modulo int1 int2))))

;; Define Euclid-list
;; This is very similar to EuclidAlgo but it keeps track of
;; all values of int1. So the output should be a list of
;; what int1 is at all points in the algorithm.
;; The first number should be what a is initially and the
;; last number should be the gcd.

;; Examples:
;; (Euclid-list 1234 24) -> '(1234 24 10 4 2)
;; (Euclid-list 17 4) -> '(17 4 1)
;; (Euclid-list 51 100) -> '(51 100 51 49 2 1)

;; Type signature: (Euclid-list int int) -> int-list

(define (Euclid-list int1 int2)
  (elh int1 int2 '()))

(define (elh int1 int2 list)
      (if (zero? int2)
          (cons int1 list)
          (elh int2 (modulo int1 int2) (cons int1 list))))

;; Define wj-help
;; This should be an implementation of the water jug problem with 2 jugs.
;; This should be a cond statement (or a messy nested if)
;; There are 5 cases to consider. 1: fillB = goal 2: fillB = maxB
;; 3: fillA = 0 4: fillA + fillB > maxB 5: fillA + fillB <= maxB
;; Remember the 3 procedures are empty B, fill A, pour A into B
;; You should make a list that contains the pair (fillA fillB)
;; at each step of the recursion
;; To make a pair and insert it in the list you should do (cons (list thing1 thing2) rest-of-list)
;; Make sure that the final pair is a pair instead of 2 individual elements
;; You can assume that the bigger jug is always second and that the solution is possible

;; Examples:
;; (waterjug 3 5 4) -> '((0 0) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4))
;; (equal? (waterjug 6 17 14) '((0 0)(6 0)(0 6)(6 6)(0 12)(6 12)(1 17)(1 0)(0 1)(6 1)(0 7)(6 7)(0 13)(6 13)(2 17)(2 0)(0 2)(6 2)(0 8)(6 8)(0 14))) 

(define (wj-help fillA fillB maxA maxB goal)
  (cond ((= goal fillB)
         (cons (list fillA fillB) '()))
        ((= fillB maxB)
         (cons (list fillA fillB) (wj-help fillA 0 maxA maxB goal)))
        ((= fillA 0)
         (cons (list fillA fillB) (wj-help maxA fillB maxA maxB goal)))
        ((> (+ fillA fillB) maxB)
         (cons (list fillA fillB) (wj-help (- fillA (- maxB fillB)) maxB maxA maxB goal)))
        ((<= (+ fillA fillB) maxB)
         (cons (list fillA fillB) (wj-help 0 (+ fillA fillB) maxA maxB goal)))))
    
      
      

(define (waterjug jugA jugB goal)
  (wj-help 0 0 jugA jugB goal))
