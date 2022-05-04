#lang racket
(provide DNN Layer Fit Predict)
(require "derivative.rkt")

(define (Layer no-of-neurons)  
  (list no-of-neurons (map (lambda(x) 0) (enumerate-interval 1 no-of-neurons)) )
  )

(define (enumerate-interval low high)
    (if (> low high)  null (cons low (enumerate-interval (+ low 1) high))))

(define (append-list list1 to-append-list)
  (cond
    ((null? list1) (list to-append-list))
    (else (append list1 (list to-append-list))))
)

(define (DNN layers)
  (define prev-no-of-neurons 1)
  (define Neurons null)
  (define Weights null)
  (map
   (lambda(x)
     (set! Neurons (append-list Neurons (car x)))
     (set! Weights (append-list Weights (map (lambda(y) 0.1) (enumerate-interval 1 (* (car x) prev-no-of-neurons)))))
     (set! prev-no-of-neurons (car x))
      )
   layers)
  (display "Model Created!!")(newline)
  (list Neurons Weights)  
  )

(define (Fit model features target epochs output-categories learning-rate)
  (display "Fit model")
  (newline)
  (display "passed model\n")
  (display model) (newline)

  (display "\npassed features:")
  (display features)(newline)
  
  (define neurons (car model))
  (define no-of-output-neurons  (list-ref neurons (- (length neurons) 1)))
  
  
  (define weights (cadr model))
  (define last-output null)
  (define outputs null)
  (define all-outputs null)
  (define target-outputs (map (lambda(x)
                                (define output-structure null)
                                (map (lambda(y)
                                       (set! output-structure (append-list output-structure  (cond ((= x (list-ref output-categories (- y 1))) 1.0)
                                                                           (else 0.0))))
                                       ) (enumerate-interval 1 no-of-output-neurons))
                                output-structure)
                              target))
  
  (define model-length (length weights))

  (define (epoch-loop) 
  (cond ((> epochs 0)
         (display "\nepoch:")
         (display epochs)(newline)
         (map (lambda(x)
                (set! last-output (forward-propagation weights model-length x))
                (set! all-outputs (append-list all-outputs (cadr last-output)))
                (set! outputs (append-list outputs (car last-output)))                
         ) features)
  (set! weights (backward-propagation outputs target-outputs all-outputs neurons weights model-length learning-rate))
  (set! epochs (- epochs 1))
  (set! outputs null)
  (set! all-outputs null)
  (epoch-loop)
  )
         ))

  (epoch-loop) 

   (list neurons weights)  
  
  )



(define (softmax-activation prev-layer-neurons)
  
  (define total-exponentials (accumulate + 0 (map (lambda(y) (exp y)) prev-layer-neurons)))
  ;(display "\ntotal-exponentials:")
  ;(display total-exponentials) (newline)
  (map (lambda(x)(/ (exp x) total-exponentials))  prev-layer-neurons)
  )

(define (forward-propagation weights model-length features)
  (define count 0)
  (define prev-layer-neurons null)
  (define all-neurons null)
  
  
  (define (propagate layered-weights)
    (cond
      ((= count 0)
       (set! count (+ count 1))
       (set! prev-layer-neurons  (map (lambda(x y) (* x y)) features (car layered-weights)))
       (set! all-neurons (append-list all-neurons prev-layer-neurons))
       ;(display "\nNew prev-layer-neurons in first conditional:")
       (display prev-layer-neurons) (newline)
       (propagate (cdr layered-weights)))
      ((< count model-length)       
       (set! count (+ count 1))
       (set! prev-layer-neurons (softmax-activation (map (lambda(x)(accumulate + 0 (map (lambda(y) (* x y) ) (car layered-weights)))) prev-layer-neurons)))
       ;(display "\nNew prev-layer-neurons in second conditional:")
       (display prev-layer-neurons) (newline)
       (set! all-neurons (append-list all-neurons prev-layer-neurons))
       (propagate (cdr layered-weights))
       )
      (else 
            (list prev-layer-neurons all-neurons)            
            )
      )
    )
  (propagate weights)
  
  )

(define (accumulate op initial sequence)
  ;(display "\nInside accumulate")
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(define (cross-entropy model-probability target-probability)
  (* -1 (accumulate + 0 (map (lambda(x y) (* (cond ((= x 0) 0) (else (log x))) y) ) model-probability target-probability)))
  )

(define Af '(/ (exp Z) (summation (exp Z) n)))
(define Ef '(* -1 (summation (* y (log Af)) n)))
(define E '(* -1 (summation (* y (log Z)) n)))
(define E0 '(* -1 (summation (* y (log Z0)) n)))
(define Z '(summation (* w x) n))
(define Z0 '(* w x) )

(define all-functions (list 'Af 'Ef 'E 'E0 'Z 'Z0))

(define (check-list-element list1 element)
(define (check list1)
  (cond
    ((null? list1) '#f)
    ((eq? (car list1) element) (car list1))
    (else (check (cdr list1)))
    ))
  (check list1)
  )

(define (make-fraction numerator denominator)
  (display "\nInside make-fraction")(newline)
  (display "passed numerator: ")
  (display numerator) (newline)
  (display "passed denominator: ")
  (display denominator) (newline)
  '(/ numerator denominator))

(define (chain-rule numerator-function denominator-function error)
  (display "numerator function :")
  (display numerator-function) (newline)
  (display "denominator function  :")
  (display denominator-function) (newline)

  (define numerator-elements (fringe numerator-function))
  (display "numerator-elements: ")
  (display numerator-elements)(newline)

  (display "all functions:")
  (display all-functions) (newline)

  (define whole-chain-rule (list '*))

 

  (define (find-all-derivatives numerator-elements)
    (display "inside find all derivatives of numerator elements")(newline)
    (cond ((check-list-element numerator-elements denominator-function)
           (display "\ncondition passed")(newline)
           (set! whole-chain-rule (append-list whole-chain-rule (deriv numerator-function denominator-function))))
          (else
           (display "\nelse condition")(newline)
           (define current-denominator null)
           (map
            (lambda(x)
              (cond ((check-list-element numerator-elements x)
                     (set! current-denominator x)))
              )
            all-functions)
           (display "after searching for denominator: ")
           (display current-denominator) (newline)
           
           (set! whole-chain-rule (append-list whole-chain-rule (deriv numerator-function current-denominator)))
           (display "after setting whole chain rule: ")
           (display whole-chain-rule) (newline)

           (display "current-denominator : next-numerator :")
           (display current-denominator) (newline)
           (cond
                ((eq? current-denominator 'Af) (set! current-denominator Af))
                ((eq? current-denominator 'Ef) (set! current-denominator Ef))
                ((eq? current-denominator 'E) (set! current-denominator E))
                ((eq? current-denominator 'E0) (set! current-denominator E0))
                ((eq? current-denominator 'Z) (set! current-denominator Z))
                ((eq? current-denominator 'Z0) (set! current-denominator Z0)))
           
           (display "set current-denominator: ")
           (display current-denominator) (newline)
           
           (define current-denominator-elements (fringe current-denominator))
           
           (display "\nCurrent denominator elements: ")
           (display current-denominator-elements) (newline)
           
           (find-all-derivatives current-denominator-elements)       
           )
           
          )
    (display "\nExecute gradient using whole chain rule ")(newline)
    (display "whole chain rule: ")
    (display whole-chain-rule)(newline)
    
    )

  (if (check-list-element numerator-elements denominator-function) (deriv (make-fraction numerator-function denominator-function))
      (find-all-derivatives numerator-elements))
  
  error
  )

(define (fringe x)
(cond ((null? x) null)
((not (pair? x)) (list x))
(else (append (fringe (car x))
(fringe (cdr x))))))

(define (gradient Error-function denominator-function layer model neurons error)
  ;(deriv Error-function denominator-function)
  (define numerator-function Error-function)
  
  (chain-rule numerator-function denominator-function error)
  )


(define (backward-propagation outputs target-outputs all-outputs neurons weights model-length learning-rate)
  (display "\nback-propagation")(newline)
  
  (define no-of-outputs (length outputs))
  
  (define error (/ (accumulate + 0 (map (lambda(x y) (cross-entropy x y) ) outputs target-outputs)) no-of-outputs))

  
  (define updated-weight null) 

  (map
   (lambda(x y z)     
  (define (update-weight layer-weight layer-count)
           
           (define layer-gradient null)
           
           (cond
             ((= layer-count 1)
              (set! layer-gradient (gradient E0 'w layer-count layer-weight (list-ref z 0) error))
              (set! updated-weight (append-list updated-weight (map (lambda(x) (- x (* learning-rate layer-gradient))) layer-weight)))              
              )
             ((= layer-count model-length)
              (set! layer-gradient (gradient Ef 'w layer-count layer-weight (list-ref z (- layer-count 1)) error))
              ;(display "\nAfter layer-gradient:")
              ;(display layer-gradient)
              (set! updated-weight (append-list updated-weight (map (lambda(x) (- x (* learning-rate layer-gradient))) layer-weight)))
              ;(display "\nupdated-weight:")
              ;(display updated-weight)(newline)
              (update-weight (list-ref weights (- layer-count 2)) (- layer-count 1))
              )
             ((> layer-count 0)
              (set! layer-gradient (gradient E 'w layer-count layer-weight (list-ref z (- layer-count 1)) error))
              (set! updated-weight (append-list updated-weight (map (lambda(x) (- x (* learning-rate layer-gradient))) layer-weight)))
              (update-weight (list-ref weights (- layer-count 2)) (- layer-count 1))
              )
             )
           )

   
  (update-weight (list-ref weights (- (length weights) 1)) model-length)
     (set! weights (reverse updated-weight))
  )
  outputs
  target-outputs
  all-outputs
  )
  weights
  )


(define (reverse list1)
  (define list-length (- (length list1) 1))
  (map (lambda(x) (list-ref list1 (- list-length x))) (enumerate-interval 0 list-length))
  )

(define (Predict model input)
  (display "Predict model")
  (newline)
  (display "to-predict input data:")
  (display input)
  )