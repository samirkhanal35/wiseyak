#lang r6rs
(library (list-tools setops (1))
  (export set empty-set empty-set? list->set set->list
          union intersection difference member?)
  (import (rnrs base) (only (rnrs lists) member))

  (define-syntax set
    (syntax-rules ()
      [(_ x ...)
       (list->set (list x ...))]))

  (define empty-set '())

  (define empty-set? null?)

  (define list->set
    (lambda (ls)
      (cond
        [(null? ls) '()]
        [(member (car ls) (cdr ls)) (list->set (cdr ls))]
        [else (cons (car ls) (list->set (cdr ls)))])))

  (define set->list (lambda (set) set))

  (define u-d-help
    (lambda (s1 s2 ans)
      (let f ([s1 s1])
        (cond
          [(null? s1) ans]
          [(member? (car s1) s2) (f (cdr s1))]
          [else (cons (car s1) (f (cdr s1)))]))))

  (define union
    (lambda (s1 s2)
      (u-d-help s1 s2 s2)))

  (define intersection
    (lambda (s1 s2)
      (cond
        [(null? s1) '()]
        [(member? (car s1) s2)
         (cons (car s1) (intersection (cdr s1) s2))]
        [else (intersection (cdr s1) s2)])))

  (define difference
    (lambda (s1 s2)
      (u-d-help s1 s2 '())))

  (define member-help?
    (lambda (x s)
      (and (member x s) #t)))

  (define-syntax member?
    (syntax-rules ()
      [(_ elt-expr set-expr)
       (let ([x elt-expr] [s set-expr])
         (and (not (null? s)) (member-help? x s)))])))