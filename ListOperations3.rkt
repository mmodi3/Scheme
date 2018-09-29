#lang eopl
;; This lab focuses on creating tail recursive function.
;; The function is given and the helper must be completed


;; Define reverse-help
;; Given a list and an empty list, return the reversed list without having to go back up the stack
;; The reverse list should be getting created in the outlist while elements are taken from the inlist

;; Examples:
;; (reverse '(1 2 3 4 5)) => '(5 4 3 2 1)
;; (reverse '(1 3 1 3)) => '(3 1 3 1)
;; (reverse '()) => '()
;; (reverse '(1)) => '(1)

;; Type signature: (reverse-help list list) -> list

(define (reverse-help inlist outlist)
  (if (null? inlist)
      outlist
      (reverse-help (cdr inlist) (cons (car inlist) outlist))))

(define (reverse list)
  (reverse-help list '()))

;; Define max-help
;; Given a list of numbers return the biggest number in the list
;; The running-max should be the current max at all times

;; Examples:
;; (max '(0 1 2 3 4 5 6)) => 6
;; (max '(2 1 2 2)) => 2
;; (max '(32 56 12 -3)) => 56
;; (max '(-246 -17 -534 0 -8)) => 0
;; (max '(1)) => 1
;; (max '()) => -inf.0

;; Type signature: (max-help list-of-ints int) => int

(define (max-help numbers running-max)
  (if (null? numbers)
      running-max
      (if (> (car numbers) running-max)
          (max-help (cdr numbers) (car numbers))
          (max-help (cdr numbers) running-max))))

(define (max numbers)
  (max-help numbers -inf.0))


;; Define min-help
;; Given a list of numbers return the smallest number in the list
;; The running-min should be the current min at all times

;; Examples:
;; (min '(0 1 2 3 4 5 6)) => 0
;; (min '(2 1 2 2)) => 1
;; (min '(32 56 12 -3)) => -3
;; (min '(-246 -17 -534 0 -8)) => -534
;; (min '(1)) => 1
;; (min '()) => +inf.0

;; Type signature: (min-help list-of-ints int) => int

(define (min-help numbers running-min)
  (if (null? numbers)
      running-min
      (if (< (car numbers) running-min)
          (min-help (cdr numbers) (car numbers))
          (min-help (cdr numbers) running-min))))


(define (min numbers)
  (min-help numbers +inf.0))

;; Define sum-help
;; Given a list of numbers return the sum of the numbers in the list
;; The running-total should be the current total at all times

;; Examples:
;; (sum '(0 1 2 3 4 5 6)) => 21
;; (sum '(2 1 2 2)) => 7
;; (sum '(32 56 12 -3)) => 97
;; (sum '(-246 -17 -534 0 -8)) => -805

;; Type signature: (sum-help list-of-ints int) => int

(define (sum-help numbers running-total)
  (if (null? numbers)
      running-total
      (sum-help (cdr numbers) (+ running-total (car numbers)))))


(define (sum numbers)
  (sum-help numbers 0))


;; Define reduce-help
;; Given a list of elements (not necessarily numbers), and "folds" them
;; That is, (reduce f init (x1 x2 x3 ... xn)) will be the same as
;; (f x1 (f x2 (f x3 (... (f xn init)))))
;; In order to do the nesting, we will pass the list in reverse order
;; and change the value of init at each step of the recursion to the new
;; "running total"

;; Examples
;; (reduce + 0 '(1 2 3)) => 6
;; (reduce append '() '((1 2 3) (a b) (2.0 4.5))) => '(1 2 3 a b 2.0 4.5)

;; Type signature (reduce-help fn b a-list) -> b
;; where fn is a function of type signature (fn a b) -> b

(define (reduce-help f init list)
  (if (null? list)
      init
      (reduce-help f (f init (car list)) (cdr list))))


(define (reduce f init list)
  (reduce-help f init list))


;;__________________________________________________________________
;;__________________________________________________________________
;; Challenge Problem
;; This is an optional problem that is worth 1 point
;; If it attempted your submission must still compile

;; Define product using reduce from above
;; This function should only contain a call to reduce and nothing else

;; Examples:
;; (product '(1 2 3 4 5)) => 120
;; (product '()) => 1

;; Type signature: (product list-of-ints) -> int

(define (product numbers)
  (reduce * 1 numbers))
