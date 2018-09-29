#lang eopl

;; List processing and higher order functions in Scheme

;; A large portion of Lisp programming is defining things using lists
;; (like we have all semester) and higher order functions. A higher
;; order function is a function which either takes in a function as
;; it's argument or returns a function. In this lab we will be using
;; list processing functions and higher order functions in Scheme.


;; lambda is a piece of Scheme's syntax which allows the programmer to
;; easily define a function without giving it a name. For example to
;; define a doubling function we can write

(define (double x)
  (* x 2))

;; But if we didn't want to give the function a name we could also
;; define it like so.

(lambda (x) (* x 2))

;; You can either define helper functions using define or lambda in
;; this lab.


;; Map is used to apply a function to every element in a list.

;; (map null? '(() (1 2 3) () (a b))) => (#t #f #t #f)
;; (map double '(1 2 3 4 5)) => (2 4 6 8 10)
;; (map (lambda (x) (equal? x 3)) '(1 2 3 1 2 3)) => '(#f #f #t #f #f #t)


(define (map f list)
  (if (null? list)
      '()
      (cons (f (car list)) (map f (cdr list)))))


;; Define a function triple-all which given a list of numbers returns
;; a new list of 3 times every number in the original list. Use map,
;; but you can define the helper function using either define or
;; lambda.

;; (triple-all '(1 2 3)) => '(3 6 9)
;; (triple-all '(-5 7 10 -23)) => '(-15 21 30 -69)

(define (triple-all numbers)
  (map (lambda (x) (* x 3)) numbers))

            


;; Define a function make-pairs which given an element `x` and a list
;; returns the list of all pairs (x, y) for each y in the list. Use
;; map, you can define the helper method using either lambda or with a
;; named function.

;; (make-pairs 1 '(a b c)) => '((1 a) (1 b) (1 c))
;; (make-pairs 'x '((1 2) (4 6))) => '((x (1 2)) (x (4 6)))


(define (make-pairs x lst)
  (map (lambda (y) (append (cons x(cons y '())) '())) lst))


;; Filter is used to select elements of a list that have a certain
;; property.

;; (filter odd? '(1 2 3 4 5 6 7 8 9)) => '(1 3 5 7 9)
;; (filter (lambda (x) (> x 5)) '(1 20 3 40 5 60 7)) => '(20 40 60 7)


(define (filter pred list)
  (cond
    [(null? list) '()]
    [(pred (car list)) (cons (car list) (filter pred (cdr list)))]
    [else (filter pred (cdr list))]))


;; Define a function keep-evens which given a list of numbers returns
;; a new list containing only the even numbers in the list. Use
;; filter.

;; (keep-evens '()) => '()
;; (keep-evens '(2)) => '(2)
;; (keep-evens '(1 3)) => '()
;; (keep-evens '(1 2 3 4 5 6 7 8 9 10)) => '(2 4 6 8 10)

(define (keep-evens numbers)
  (filter (lambda (x) (= (modulo x 2) 0)) numbers))


;; Folding is where you take a sequence of values and combine them
;; together into a single value (this is sometimes referred to as
;; "reducing"). For example, the sum of a sequence can be described as
;; folding a list of numbers using +. In this lab we will be use fold
;; via a function we will call fold-left.

;; (fold-left + 0 '(1 2 3 4)) => 10
;; (fold-left * 1 '(1 2 3 4 5)) => 120
;; (fold-left (lambda (list el) (cons el list)) '() '(1 2 3)) => '(3 2 1)

(define (fold-left f init list)
  (if (null? list)
      init
      (fold-left f
                 (f init (car list))
                 (cdr list))))


;; Define a function concat which when given a list of lists
;; concatenates all of them together. You should use fold-left in your
;; definition.

;; (concat '()) => '()
;; (concat '((1))) => '(1)
;; (concat '((1) (2 3) (4 5 6 7))) => '(1 2 3 4 5 6 7)


(define (concat lists)
  (fold-left append '() lists))


;; Define a function cartesian-product which when given two lists
;; returns their Cartesian (cross) product. Order does not matter. You
;; should use concat and make-pairs.

;; (cartesian-product '() '()) => '()
;; (cartesian-product '() '(1 2)) => '()
;; (cartesian-product '(1 2 3) '(a b c)) => '((1 a) (1 b) (1 c)
;;                                            (2 a) (2 b) (2 c)
;;                                            (3 a) (3 b) (3 c))


(define (cartesian-product xs ys)
   (concat (map (lambda (x) (map (lambda (y) (list x y)) ys)) xs)))