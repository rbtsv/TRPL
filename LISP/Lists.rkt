#lang racket

(define lst (cons 1 2))

(displayln lst)

(define lst2 (cons 0 lst))

(displayln (cons lst 3))
(displayln lst2)

(car lst)
(car lst2)
(cdr lst)
(cadr lst2) ; (car (cdr x))

(define true-lst (cons 1 (cons 2 null)))
true-lst
(define true-lst3 '(1 2 3))
true-lst3

(cadr true-lst)
(cdr (cdr true-lst))

(+ 1 2 3 4 5)

(define (sum x y . lst)
  (define (_sum _lst)
    (if (null? _lst)
        0
        (+ (car _lst) (_sum (cdr _lst)))
     ))
  (+ x y (_sum lst)) 
)
(sum 1 2 3 4 5)