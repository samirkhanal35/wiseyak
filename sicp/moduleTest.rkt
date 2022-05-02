#lang racket

(provide sample-sum sample-sum2)
(define sample-sum
  (lambda (x y) (+ x y)))

(define (sample-sum2 x y)
  (+ x y))