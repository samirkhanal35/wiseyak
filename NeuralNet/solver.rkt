#lang racket
(provide solve)

(define (solve expressions)
  (define expression (car expressions))
  (define passed-values (cdr expressions))

  (define Af (cadr expressions))
  (define Ef (cadr (cdr expressions)))
  (define E (cadr (cdr (cdr expressions))))
  (define E0 (cadr (cdr (cdr (cdr expressions)))))
  (define Z (cadr (cdr (cdr (cdr (cdr expressions))))))
  (define Z0 (cadr (cdr (cdr (cdr (cdr (cdr expressions)))))))
  (define n (cadr (cdr (cdr (cdr (cdr (cdr (cdr expressions))))))))
  (define y (cadr (cdr (cdr (cdr (cdr (cdr (cdr (cdr expressions)))))))))

  (define (variable-values x)
   
    (cond
      ((eq? x 'Af) Af)
      ((eq? x 'Ef) Ef)
      ((eq? x 'E) E)
      ((eq? x 'E0) E0)
      ((eq? x 'Z) Z)
      ((eq? x 'Z0) Z0)
      ((eq? x 'n) n)
      ((eq? x 'y) y)
      )
    )  

(define (square x) (* x x) )
  
(define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)  null (cons low (enumerate-interval (+ low 1) high))))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
((=number? a2 0) a1)
(else
(+ a1 a2))
))

(define (make-difference a1 a2)
(cond ((=number? a1 0) (* -1 a2))
((=number? a2 0) a1)
(else
(- a1 a2))
))

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
((=number? m1 1) m2)
((=number? m2 1) m1)
(else (* m1 m2))
))

(define (make-division m1 m2)
(cond ((or (= m1 0) (= m2 0)) 0)
(else
 (/ m1 m2))
))

  
(define (make-summation expr final-count)
 
  (cond
    ((number? final-count)
     (cond
((=number? final-count 0) 0)
(else
 (accumulate-summation + 0 (map (lambda(x) expr) (enumerate-interval 0 final-count))))
)
     )
  )
  )

(define (make-logarithmic expr1) (log expr1) )

(define (accumulate-summation op initial sequence) 
   (if (null? sequence)
       initial
       (cond
         ((number? (car sequence))
                   (op (car sequence)
           (accumulate-summation op initial (cdr sequence)))
         )         
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

  (define (solver expr)
(cond
  ((number? expr) expr)
  ((sum? expr) (make-sum (solver (addend expr))
                             (solver (augend expr))))
  ((product? expr) (make-product (solver (multiplier expr)) (solver (multiplicand expr))))
  ((division? expr)
   
   (make-division (solver (get-numerator expr)) (solver (get-denominator expr) ) ) )
                    
  ((summation? expr) (make-summation (solver (expression? expr) ) (solver (get-final-summation-value expr))))
  ((exponential? expr) (expression? expr))
  ((logarithmic? expr) (make-logarithmic (expression? expr)))
  ((not (number? expr))
   
   (cond ((not (list? expr))
          
          (define expr-value (variable-values expr))
         
          expr-value
          ) (else (solver expr))))
  ))
  (solver expression)
)


;(* (* -1 (summation (* y (/ 1 Af)) n)) (* -1 (summation (* y (/ 1 Af)) n)) (* -1 (summation (* y (/ 1 Af)) n)))

