#lang racket
(provide DNN Layer Fit Predict)
(require "derivative.rkt")
(require "solver.rkt")

(define (Layer no-of-neurons)  
  (list no-of-neurons (map (lambda(x) 0) (enumerate-interval 1 no-of-neurons)) )
  )

(define (enumerate-interval low high)
    (if (> low high)  null (cons low (enumerate-interval (+ low 1) high))))

(define (append-list list1 to-append-list)
  (cond
    ((null? list1) (list to-append-list) )
    (else (append list1 (list to-append-list))))
)

(define (DNN layers)
  (define prev-no-of-neurons 1)
  (define Neurons null)
  (define Weights null)
  (map
   (lambda(x)
     (set! Neurons (append-list Neurons (car x)))
     (set! Weights (append-list Weights (map (lambda(y) 0.01) (enumerate-interval 1 (* (car x) prev-no-of-neurons)))))
     (set! prev-no-of-neurons (car x))
      )
   layers)
  (display "Model Created!!")(newline)
  (list Neurons Weights)  
  )

(define (Fit model features target epochs output-categories learning-rate)
    
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
                (display "\nOutput from forward propagation: ")(newline)
                (display last-output) (newline) (newline)
                (set! all-outputs (append-list all-outputs (cadr last-output)))
                (set! outputs (append-list outputs (car last-output)))                
         ) features)
  (set! weights (backward-propagation outputs target-outputs all-outputs neurons weights model-length learning-rate))
  (set! epochs (- epochs 1))
  (set! outputs null)
  (set! all-outputs null)
  (epoch-loop)
  )
         )
    )

  (epoch-loop) 

   (list neurons weights) 
  
  )

(define (softmax-activation prev-layer-neurons)
  (display "\nInside softmax-activation function") (newline)
  (display "passed last layer neurons: ")
  (display prev-layer-neurons) (newline)
  
  (define total-exponentials (accumulate + 0 (map (lambda(y) (exp y)) prev-layer-neurons)))
  
  (map (lambda(x)(/ (exp x) total-exponentials))  prev-layer-neurons)
  )

(define (forward-propagation weights model-length features)
  (define count 0)
  (define prev-layer-neurons null)
  (define all-neurons null)

  (display "\nInside forward propagation") (newline)
  (display "passed model-length: ")
  (display model-length) (newline) (newline)
  (display "passed features: ")
  (display features) (newline) (newline)
  (display "passed weights: ")
  (display weights) (newline)
  (newline)
  
  (define (propagate layered-weights)
    (display "\nInside propagate layered weights")(newline)
    (display "layered weights: ")
    (display layered-weights) (newline) (newline)
    (define step-size 0)
      
    (cond
      ((= count 0)
       (display "Inside condition count = 0")(newline)
       (set! count (+ count 1))
       (set! prev-layer-neurons  (map (lambda(x y) (* x y)) features (car layered-weights)))
       (set! all-neurons (append-list all-neurons prev-layer-neurons))
       (propagate (cdr layered-weights)))
      ((< count (- model-length 1))
       (display "Inside condition count < model length -1 ") (newline)
       (set! count (+ count 1))
       
       (display "\nlength of layered weights: ")
       (display (length layered-weights)) (newline)

       
       (set! step-size (/ (length (car layered-weights)) (length prev-layer-neurons)))
       (display "\nstep-size: ")
       (display step-size) (newline)
       
       (set! prev-layer-neurons (map
                                 (lambda(x)
                                   (define step-count 0)
                                   (accumulate + 0 (map
                                                    (lambda(y)
                                                      (define temp-value (* (list-ref prev-layer-neurons (- y 1)) (list-ref (car layered-weights) (+ (- x 1) step-count))) )
                                                      (set! step-count (+ step-count step-size))
                                                      temp-value
                                                      )
                                                    (enumerate-interval 1 (length prev-layer-neurons))
                                                    )
                                               )
                                   )
                                 (enumerate-interval 1 step-size) 
                                 )
             )
       
       (set! all-neurons (append-list all-neurons prev-layer-neurons))
       (propagate (cdr layered-weights)))
      ((= count (- model-length 1))
       (display "Inside condition count = model length -1") (newline)
       (set! count (+ count 1))
       (set! step-size (/ (length (car layered-weights)) (length prev-layer-neurons)))
       (display "\nstep-size: ")
       (display step-size) (newline)
       (set! prev-layer-neurons (softmax-activation
                                 (map
                                 (lambda(x)
                                   (define step-count 0)
                                   (accumulate + 0 (map
                                                    (lambda(y)
                                                      (define temp-value (* (list-ref prev-layer-neurons (- y 1)) (list-ref (car layered-weights) (+ (- x 1) step-count))) )
                                                      (set! step-count (+ step-count step-size))
                                                      temp-value
                                                      )
                                                    (enumerate-interval 1 (length prev-layer-neurons))
                                                    )
                                               )
                                   )
                                 (enumerate-interval 1 step-size) 
                                 )
                                 ))
       (set! all-neurons (append-list all-neurons prev-layer-neurons))
       ;(propagate (cdr layered-weights))
       )
      )
    (list prev-layer-neurons all-neurons)
    )
  (propagate weights)  
  )

(define (accumulate op initial sequence)
  
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
  '(/ numerator denominator))

(define (chain-rule numerator-function denominator-function error layer-neuron-outputs)
  
  (define numerator-elements (fringe numerator-function))
 
  (define whole-chain-rule (list '*))

  (define (find-all-derivatives numerator-elements)
    
    (cond ((check-list-element numerator-elements denominator-function)
          
           (set! whole-chain-rule (append-list whole-chain-rule (deriv numerator-function denominator-function))))
          (else
           
           (define current-denominator null)
           (map
            (lambda(x)
              (cond ((check-list-element numerator-elements x)
                     (set! current-denominator x)))
              )
            all-functions)
           
           (set! whole-chain-rule (append-list whole-chain-rule (deriv numerator-function current-denominator)))
           
           (cond
                ((eq? current-denominator 'Af) (set! current-denominator Af))
                ((eq? current-denominator 'Ef) (set! current-denominator Ef))
                ((eq? current-denominator 'E) (set! current-denominator E))
                ((eq? current-denominator 'E0) (set! current-denominator E0))
                ((eq? current-denominator 'Z) (set! current-denominator Z))
                ((eq? current-denominator 'Z0) (set! current-denominator Z0)))
           
          
           
           (define current-denominator-elements (fringe current-denominator))
           
          
           (find-all-derivatives current-denominator-elements)       
           )
           
          )
    
    
    )

  (if (check-list-element numerator-elements denominator-function) (deriv (make-fraction numerator-function denominator-function))
      (find-all-derivatives numerator-elements))

  ;(display "whole chain rule: ")
  ;(display whole-chain-rule)(newline)

  ;solve the expression base chain-rule
  (define Af1 1)
  (define Ef1 1)
  (define E1 1)
  (define E01 1)
  (define Z1 1)
  (define Z01 1)
  (define n (length layer-neuron-outputs))
  (define y 1)
  (define gradient-solution (solve (list whole-chain-rule Af1 Ef1 E1 E01 Z1 Z01 n y)))
 
  
  ;error
  gradient-solution
  )

(define (fringe x)
(cond ((null? x) null)
((not (pair? x)) (list x))
(else (append (fringe (car x))
(fringe (cdr x))))))

(define (gradient Error-function denominator-function layer-count layer-weights layer-neuron-outputs error)
 
  (define numerator-function Error-function)   
  (chain-rule numerator-function denominator-function error layer-neuron-outputs)
  )


(define (backward-propagation outputs target-outputs all-outputs neurons weights model-length learning-rate)  
  (define no-of-outputs (length outputs))   
  (define error (/ (accumulate + 0 (map (lambda(x y) (cross-entropy x y) ) outputs target-outputs)) no-of-outputs))  
  (define updated-weight null)
  (define all-layers-gradient null)
  (define last-layer-gradient null)

  (map
   (lambda(x y z)     
  (define (update-weight layer-weight layer-count)           
           (define layer-gradient null)           
           (cond
             ((= layer-count 1)
              (set! layer-gradient (gradient E0 'w layer-count layer-weight (list-ref z 0) error))
              (set! layer-gradient (* layer-gradient last-layer-gradient))
              (set! last-layer-gradient layer-gradient)
              (set! updated-weight (append-list updated-weight (map (lambda(x) (- x (* learning-rate layer-gradient))) layer-weight)))              
              )
             ((= layer-count model-length)
              (set! layer-gradient (gradient Ef 'w layer-count layer-weight (list-ref z (- layer-count 1)) error))
              (set! last-layer-gradient layer-gradient)        
              (set! updated-weight (append-list updated-weight (map (lambda(x) (- x (* learning-rate layer-gradient))) layer-weight)))
             
              (update-weight (list-ref weights (- layer-count 2)) (- layer-count 1))
              )
             ((> layer-count 0)
              (set! layer-gradient (gradient E 'w layer-count layer-weight (list-ref z (- layer-count 1)) error))              
              (set! layer-gradient (* layer-gradient last-layer-gradient))
              (set! last-layer-gradient  layer-gradient)
              (set! updated-weight (append-list updated-weight (map (lambda(x) (- x (* learning-rate layer-gradient))) layer-weight)))
              (update-weight (list-ref weights (- layer-count 2)) (- layer-count 1))
              )
             )
           )
     
  (update-weight (list-ref weights (- (length weights) 1)) model-length)
  (set! weights (reverse updated-weight))
  (set! updated-weight null)
  (set! all-layers-gradient null)
  (set! last-layer-gradient null)
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
  (display "\nInside Predict model")
  (newline)
  (display "passed model: ")(newline)
  (display model) (newline) (newline)
  (display "to-predict input data: ")
  (display input) (newline) (newline)

  (define weights (cadr model))
  (define model-length (length (car model)))

  (display "weights: ")
  (display weights) (newline) (newline)

  (display "model-length: ")
  (display model-length) (newline) (newline)

  (display "returned value from forward propagation")
  (display (forward-propagation weights model-length input))
  
  )