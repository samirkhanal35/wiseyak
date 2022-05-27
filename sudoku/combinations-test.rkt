#lang racket
(define list1 (list 1 2 3 4))

(define all-combinations null)

(define (condition-check value)
  (cond
    ((< value 45)
     '#t
    )
    (else
     '#f
     )
    )
  )

(define (accumulate op initial sequence)  
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(define (append-list list1 to-append-list)
  (cond
    ((null? list1) (list to-append-list))
    (else (append list1 (list to-append-list))))
    )

(define (append list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append (cdr list1) list2))))

(define (combination-generator list1 initial-index combination-length)
  (define initial-value (list-ref list1 initial-index))
  (define temp-combine null)
  (define temp-combination-length combination-length)
  (define temp-combinations null)
  (define prev-combinations null)
  
  (define (combinator initial-value)
    (cond
      ((not(list? initial-value)) (set! initial-value (list initial-value)))
          )
    (cond
      ((< temp-combination-length (+ (length list1) 1))
       (set! temp-combine (append-list initial-value (list-ref list1 (- temp-combination-length 1))))
    ;(display "temp-combine: ")
    ;(display temp-combine) (newline)
    (set! temp-combinations (append-list temp-combinations temp-combine))
    
    (cond
      ((condition-check (accumulate * 1 temp-combine))
       (set! temp-combination-length (+ temp-combination-length 1))
       (combinator initial-value)
       )
      (else
       (set! temp-combination-length (+ temp-combination-length 1))
       (combinator initial-value)
       )
      )
       )
      (else
       (cond
         ((< combination-length (+ (length list1) 1))
          (set! initial-value (append-list initial-value (list-ref list1 (- combination-length 1))))
          (set! combination-length (+ combination-length 1))
          (set! temp-combination-length combination-length)
          (set! all-combinations (append-list all-combinations temp-combinations))
          (set! temp-combinations null)
          (combinator initial-value)
           )          
         )
       )      
      )    
    ) 

  (combinator initial-value)
  (set! all-combinations (append-list all-combinations temp-combinations))

  ;(display "all combinations: ")
  ;(display all-combinations)(newline)
  )

(define (generate-combinations initial-index combination-length)
  (cond
    ((< combination-length (+ (length list1) 1))
     (combination-generator list1 initial-index combination-length)
     (generate-combinations (+ initial-index 1) (+ combination-length 1))
     )
    )  
  )
;(generate-combinations 0 2)
;(display "\nAll combinations: ")
;(display all-combinations) (newline)

(define (check-list-element list1 element)
   
    (define (check list1)
      (cond
        ((null? list1) '#f)
        ((equal? (car list1) element) '#t)
        (else (check (cdr list1)))
        ))
    (check list1)
    )

(define (get-unique-elements prev-elements curr-elements)
  (define new-unique-elements prev-elements)
        (map
         (lambda(x)
           (cond
             ((not (check-list-element prev-elements x)) 
              (set! new-unique-elements (append-list new-unique-elements x))
              )
             )
           )
         curr-elements
         )
       new-unique-elements
        )

(define (check-state temp-comb new-value combination-length sub-branch-combinations)
  (define unique-combinations (get-unique-elements temp-comb new-value))
  (display "\nInside check state") (newline)

  (display "passed sub branch combinations values: ")
  (display sub-branch-combinations) (newline)

  (display "created unique combinations: ")
  (display unique-combinations) (newline)

  (display "result of (check-list-element sub-branch-combinations unique-combinations): ")
  (display (check-list-element sub-branch-combinations unique-combinations)) (newline)
  
  (cond
    ((and (= (length unique-combinations) combination-length) (= (length sub-branch-combinations) 0))
     unique-combinations
     )
    ((and (= (length unique-combinations) combination-length) (not(check-list-element sub-branch-combinations unique-combinations)))
     unique-combinations)
    (else
     '#f)
    )  
  )


; New tree generator
(define (tree-generator list1)
  ;(display "\nInside tree generator") (newline)
  ;(display "passed list: ")
  (display list1) (newline)
  
  (define total-length (length list1))
  (define starting-index 0)
  (define total-combinations null)
  (define combination-length 2)
  (define prev-combinations list1)
  (define sub-branch-combinations null)

  (define (combination-generator)
    ;(display "\nInside combination generator")(newline)
    (display "\nPrev combinations: ") 
    (display prev-combinations) (newline)
    
    
    (define (sub-combination-generator)
      (display "\nInside sub combination generator") (newline)
      (define initial-value (list-ref prev-combinations starting-index))
      (display "initial value: ")
      
      (display initial-value) (newline)
      (define temp-comb null)
      (cond
        ((list? initial-value)
         (set! temp-comb initial-value)
         )
        (else
         (set! temp-comb (list initial-value))
         )
        )
      
      ;(display "temp-comb : ")
      ;(display temp-comb) (newline)
      
      (define sub-index (+ starting-index 1))
      (define count sub-index)

      (define length-count sub-index)
      
      (define (inner-sub-combination)
        ;(display "\nInside inner sub combination generator") (newline)
        (define sub-count 1)
        
        (define (combination-length-loop)
          ;(display "\nInside combination length loop") (newline)
          ;(display "sub count: ")
          ;(display sub-count) (newline)
          ;(display "count: ")
          ;(display count) (newline)
          (cond
            ((and (< sub-count combination-length) (< count (length prev-combinations)))
             (define new-value (list-ref prev-combinations count))
             (cond
               ((not(list? new-value))
                (set! new-value (list new-value))
                )
               )
             (define temp-comb-status (check-state temp-comb new-value combination-length sub-branch-combinations))
             (display "created combination: ")
             (display temp-comb-status) (newline)
                
             (cond
               (temp-comb-status
                (set! temp-comb temp-comb-status)

                (display "verified combination: ")
                (display temp-comb) (newline)
                
                (set! sub-branch-combinations (append-list sub-branch-combinations temp-comb))
                
                )
               )        
             
             (set! count (+ count 1))
             
             (cond
               ((list? initial-value)
                (set! temp-comb initial-value)
                )
               (else
                (set! temp-comb (list initial-value))
                )
               )
             (set! sub-count (+ sub-count 1))
             (combination-length-loop)
             )
            )
          )

        (combination-length-loop)

        (set! length-count (+ length-count 1))

        ;(display "length count: ")
        ;(display length-count) (newline)
        
        (cond
          ((< length-count total-length)
           (inner-sub-combination)
           )
         )                
        )      
      (inner-sub-combination)

      (set! starting-index (+ starting-index 1))

      (display "starting index: ")
      (display starting-index) (newline)

      (display "length of prev combinations: ")
      (display (length prev-combinations)) (newline)
      
      (cond
        ((< starting-index (- (length prev-combinations) 1))
         (sub-combination-generator)
         )
        )
      
      )
    (sub-combination-generator)
    
    (display "final sub branch combinations: ")
    (display sub-branch-combinations) (newline)

    (set! prev-combinations sub-branch-combinations)
    (set! sub-branch-combinations null)
    (set! starting-index 0)
    (set! total-combinations (append-list total-combinations prev-combinations))
    (set! combination-length (+ combination-length 1))

    (cond
      ((< combination-length total-length)
       (combination-generator)
       )
     )    
    )
  (combination-generator)
  (display "\nTotal combinations: ")
  (display total-combinations) (newline)
  )

(tree-generator list1)




