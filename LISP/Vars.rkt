#lang racket

;(define (sq z) (* z z z))

(define (sum_of_sq x y)
  (define (sq z) (* z z))
  (+ (sq x) (sq y))
)

(sum_of_sq 2 3)

(define (sq z) (* z z))
(sq 3)

(define (strange_of_sq x y)
  (let ((sq_x (sq x)) (sq_y (sq y)))
    (if (> y 0)
        (+ sq_x sq_y)
        (+ sq_x sq_x)
        )))
(strange_of_sq 2 3)
(strange_of_sq 2 -3)





