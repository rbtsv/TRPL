#lang racket

(define (Fib n)
 
)


(Fib 5)
(Fib 6)

#|
(define (_abs x)
) 
(_abs -7)
|#

#|
(define (gcd a b)
)

(gcd 121 33)
|#


#|
(define (new-if predicate then-clause else-clause)
  
)
(new-if (< 5 0) 1 2)
|#



#|
(define (new-gcd a b)
  (new-if (= b 0) a
      (new-gcd b (remainder a b))
      ))
(new-gcd 121 33)
|#