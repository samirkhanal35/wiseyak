#lang racket
(provide deriv)
(define (deriv expr var)
(cond
  ((number? expr) 0)
  ((variable? expr) (if (same-variable? expr var) 1 0))
  ((sum? expr) (make-sum (deriv (addend expr) var)
                             (deriv (augend expr) var)))
  ((product? expr) (make-sum
                   (make-product (multiplier expr)
                                 (deriv (multiplicand expr) var))
                   (make-product (deriv (multiplier expr) var)
                                 (multiplicand expr))))
  ((division? expr) (make-division
                     (make-difference
                      (make-product (get-denominator expr)
                                    (deriv (get-numerator expr) var))
                      (make-product (get-numerator expr)
                                    (deriv (get-denominator expr) var)))
                     (square (get-denominator expr))))
  ((summation? expr) (make-summation (deriv (expression? expr) var) (get-final-summation-value expr)))
  ((exponential? expr) (expression? expr))
  ((logarithmic? expr) (make-logarithmic (expression? expr)))
  (else
   (error "unknown expression type: DERIV" expr))
  ))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (square x)
  (cond
    ((number? x) (* x x))
    (else (list '* x x)))
  )
  
(define (accumulate op initial sequence)
  ;(display "\nInside accumulate")
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)  null (cons low (enumerate-interval (+ low 1) high))))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
((=number? a2 0) a1)
((and (number? a1) (number? a2))
(+ a1 a2))
(else (list '+ a1 a2))))

(define (make-difference a1 a2)
(cond ((=number? a1 0) (* -1 a2))
((=number? a2 0) a1)
((and (number? a1) (number? a2))
(- a1 a2))
(else (list '- a1 a2))))

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
((=number? m1 1) m2)
((=number? m2 1) m1)
((and (number? m1) (number? m2)) (* m1 m2))
(else (list '* m1 m2))))

(define (make-division m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
((=number? m1 1) (/ 1 m2))
((=number? m2 1) m1)
((and (number? m1) (number? m2)) (/ m1 m2))
(else (list '/ m1 m2))))

  
(define (make-summation expr final-count)
  (cond
    ((number? final-count)
     (cond
((=number? final-count 0) 0)
(else
 (accumulate-summation + 0 (map (lambda(x) expr) (enumerate-interval 0 final-count))))
)
     )

  (else (list 'summation expr final-count))
  )

  )

(define (make-logarithmic expr1)
(cond
((number? expr1) (/ 1 expr1))
(else
 (list '/ 1 expr1))
)
  )

(define (accumulate-summation op initial sequence)
 
   (if (null? sequence)
       initial
       (cond
         ((number? (car sequence))
                   (op (car sequence)
           (accumulate-summation op initial (cdr sequence)))
         )
         (else (list '+ (car sequence) (accumulate-summation op initial (cdr sequence))))
       )
       )
  )


(define (=number? exp num) (and (number? exp) (= exp num)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (summation? x) (and (pair? x) (eq? (car x) 'summation)))
(define (expression? p) (cadr p))
(define (get-final-summation-value p) (caddr p))

(define (exponential? x) (and (pair? x) (eq? (car x) 'exp)))

(define (logarithmic? x) (and (pair? x) (eq? (car x) 'log)))

(define (division? x) (and (pair? x) (eq? (car x) '/)))
(define (get-numerator p) (cadr p))
(define (get-denominator p) (caddr p))


;(deriv '(* x y) 'x)
;(deriv '(* (* x x) y) 'x)
;(deriv '(* (* x y) (+ x 3)) 'x)
;(deriv '(summation (* w x) 2) 'w) (newline)
;(deriv '(/ (exp Z) (summation (exp Z) 1)) 'Z) (newline)
;(deriv '(* -1 (summation (* y (log Af)) n)) 'Af) (newline)
;(deriv '(* -1 (summation (* y (log Z)) 1)) 'Z) (newline)
;(deriv '(* -1 (summation (* y (log Z0)) n)) 'Z0) (newline)


