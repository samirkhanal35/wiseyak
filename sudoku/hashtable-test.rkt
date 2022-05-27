#lang racket
(define ht (make-hash))

(hash-set! ht "apple" '(red round))

(hash-set! ht 0 'nil)

(hash-set! ht "banana" '(yellow long))

(hash-ref ht "apple")

(hash-ref ht 0)

(define count 23)

(define count-value 'nil)

(hash-set! ht count count-value)

(hash-ref ht 23)