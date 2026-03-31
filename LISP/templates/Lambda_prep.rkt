#lang racket

((lambda (x) (+ x 1)) 1)

(define (square x)
  (* x x))

(define square
  (lambda (x) (* x x)))

(define make-adder
  (lambda (n)
    (lambda (x) (+ x n))))

(let ((x 2)
      (y 3))
  (+ x y))


((lambda (x y) (+ x y)) 2 3)