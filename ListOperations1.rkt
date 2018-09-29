#lang eopl

;; Lab #3
;; Introduction to Recursion with Lists in Scheme

;; nth
;; This gets the N-th element of the list, where 0 represents the
;; first element of the list.
;; You don't need to worry about negatives or indices greater than the length
;; of the list.

;; Examples:
;; (nth 1 '(Cassidy Ed Sam Ryan)) => 'Ed
;; (nth 5 '("zero" "one" "two" "three" "four" "five")) => "five"
;; (nth 0 '(a b c)) => 'a

(define (nth n lst)
  (if (= n 0)
    (car lst)
    (nth (- n 1) (cdr lst))))
     


;; map
;; This function applies a function to every element in a list
;; Map takes a function f and a list lst. It recursively goes
;; through the list, applying f to each element in the list.

;; Examples:
;;
(define (double x) (* x 2))
;; (map zero? '(0 0 1 2)) => '(#t #t #f #f)

(define (map f lst)
   (if (null? lst)
       '()
       (cons (f (car lst)) (map f (cdr lst)))))
  
    


;; filter
;; Filter takes a predicate function (one that returns a boolean)
;; and a list. It recursively goes through the list, removing any
;; element for which (pred element) returns false and keeps any element
;; where (pred element) returns #t.

;; Examples
;; (filter zero? '(1 0 2 34 56 1 0)) => '(0 0)
;; (filter number? '(shave and 1 haircut 2 bits)) => '(1 2)

;; Type signature: (filter predicate list) -> list
;; where a predicate takes an element and returns a boolean

(define (filter pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst)(filter pred (cdr lst)))
          (filter pred (cdr lst)))))

  


;; sum
;; Returns the summation of every element in the list.

;; Type signature: (sum number-list) -> number

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst)(sum (cdr lst)))))


;; product
;; Returns the product (result of multiplying) of every element in the list.

;; (product (list 1 3 5 4)) => 60

;; Type signature: (product number-list) -> number

(define (product lst)
  (if (null? lst)
      1
      (* 1 (car lst)(product (cdr lst)))))


;; January 2018
;; Mitra Modi
;; CS 135  Discrete Structures
