#lang racket
;; Узел потока натуральных чисел, начиная с n
(define (nats-from n)
  (cons n
        (lambda ()
          (nats-from (+ n 1)))))

;; Поток натуральных чисел: 0, 1, 2, 3, ...
(define nats (nats-from 0))

;; Взять текущий элемент
(define (head stream)
  (car stream))

;; Перейти к следующему
(define (tail stream)
  ((cdr stream)))


(define s0 nats)
(head s0)   ; => 0

(define s1 (tail s0))
(head s1)   ; => 1

(define s2 (tail s1))
(head s2)   ; => 2