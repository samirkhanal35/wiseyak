#lang racket
(define (DepthFirstSearch Path nextMove checkFinalState MaxDepth update-list-position initialIndex)
  
  (define (LastStateIndex)
    (define lastIndex (- (length Path) 1))
    (define lastStateIndex (list-ref Path lastIndex))
     (list-ref lastStateIndex 1)
    )
  
  (define (firstIndex)
     initialIndex
     )
  
  (define (RemoveLastState)  
   (define lastIndex (- (length Path) 1))
   (define lastStateIndex (list-ref Path lastIndex))
   (set! Path (reverse (remove lastStateIndex (reverse Path))))
    (define newStateIndex (list (car lastStateIndex) 'nil))
   (update-list-position newStateIndex)
   (list-ref lastStateIndex 1)
   )
  
  (define (getLast-State)  
   (list-ref (list-ref Path (- (length Path) 1)) 0)
   )
  
  (define prevIndex (firstIndex))
  (define Branch-pruned '#f)
  
  (define (DFS-Inner-Loop)
  (define LastState (getLast-State))    
  (define NextStateIndex (nextMove LastState prevIndex))
  (define NextState  0)    
    
   (cond (NextStateIndex        
        (set! Path (append Path (list NextStateIndex)))
        (set! prevIndex (firstIndex))      
        (set! NextState (list-ref NextStateIndex 0))
        (cond
          ((checkFinalState NextState) (list Path Branch-pruned))
          ((> (length Path) MaxDepth)
            (set! prevIndex (RemoveLastState))
             (set! Branch-pruned '#t)
             (DFS-Inner-Loop)
             )
          (else (DFS-Inner-Loop))
         )
        )       
    (else (cond
            ((> (length Path) 1)
              (set! prevIndex (RemoveLastState))
              (DFS-Inner-Loop)
              )
           (else (list Path Branch-pruned))                               
           )
        )             
    )   
  )
  (DFS-Inner-Loop)
  )


(define (sudoku-solver sudoku-problem)

  (define (update-list-position newStateIndex)
    
    (define row-position (car (car newStateIndex)))
    (define column-position (cadr (car newStateIndex)))    
    (define newValue (cadr newStateIndex))    

    (define row (length sudoku-solution))
    (define column (length (car sudoku-solution)))    
    (define (get-updated-column-value column-list)      
      (define count 0)
      (map
       (lambda(x)
         (cond ((eq? count column-position)
               
               (set! count (+ count 1))
               newValue
               )
                 (else
                  (set! count (+ count 1))
                  x
                  ))
         )
       column-list)      
        )     

    (define row-count 0) 
     
    (set! sudoku-solution (map (lambda(x)
           (cond ((eq? row-count row-position)
                  (set! row-count (+ row-count 1))
                (get-updated-column-value x)               
               )
                 (else
                  (set! row-count (+ row-count 1))
                  x
                  ))
           ) sudoku-solution))
    
    )
  
  (define (find-next-empty-position i j)
    
    
    (define (next-i-j-values)
      (cond ((and (< i 9) (< j 8))
             (set! j (+ j 1)))
            ((and (< i 8) (= j 8))
             (set! i (+ i 1))
             (set! j 0))
            )
      )

    (cond ((or (> i 0) (> j 0))
           (set! i 0)
           (set! j 0)
           (next-i-j-values)))
    
    (define (find-i-and-jth-value)
      
      (cond
        ((and (= i 8) (= j 8))
         (cond ((eq? (list-ref (list-ref sudoku-solution i) j) 'nil)
                 (list i j) )
                 (else
                  '#f)
                 ))
            ((or (< i 9) (< j 9))
             (cond ((eq? (list-ref (list-ref sudoku-solution i) j) 'nil)
                 (list i j) )
                 (else
                  (next-i-j-values)
                 (find-i-and-jth-value))
                 )
             )            
            (else '#f)
            )
      
      )
    (find-i-and-jth-value)
    )

  (define (append-list list1 to-append-list)
  (cond
    ((null? list1) (list to-append-list))
    (else (append list1 (list to-append-list))))
    )

  (define (append-to-list list1 to-append-list)    
  (cond
    ((null? list1) (list to-append-list))
    (else (append list1 to-append-list)))
    )

  (define (enumerate-interval low high)
    (if (> low high)  null (cons low (enumerate-interval (+ low 1) high))))


  (define (fringe x)
    (cond ((null? x) null)
          ((not (pair? x)) (list x))
          (else (append (fringe (car x))
                        (fringe (cdr x))))))
  
  (define (check-list-element list1 element)
   
    (define (check list1)
      (cond
        ((null? list1) '#f)
        ((eq? (car list1) element) '#t)
        (else (check (cdr list1)))
        ))
    (check list1)
    )
  

  (define (checkFinalState state)
     (cond ((find-next-empty-position (car state) (cadr state))                    
         '#f)
           (else
         '#t
         )))
    
  (define (CheckLegality StateIndex)
     (define row-position (car (car StateIndex)))
     (define column-position (cadr (car StateIndex)))    
     (define newValue (cadr StateIndex))
     (define (box-starting-index position-value)
       (cond
         ((or (= position-value 0) (= position-value 1) (= position-value 2))
          0
          )
         ((or (= position-value 3) (= position-value 4) (= position-value 5))
          3
          )
         ((or (= position-value 6) (= position-value 7) (= position-value 8))
          6
          )         
         )
       )

     (define box-starting-row-position (box-starting-index row-position))
     (define box-starting-column-position (box-starting-index column-position))
     (define box-elements (fringe (map
      (lambda(x)
        (map
         (lambda(y) (list-ref (list-ref sudoku-solution  x) y))
         (enumerate-interval box-starting-column-position (+ box-starting-column-position 2))
         )
        )
      (enumerate-interval box-starting-row-position (+ box-starting-row-position 2))
      )))

     (define column-elements (fringe (map
      (lambda(x)
        (list-ref (list-ref sudoku-solution (- x 1)) column-position)
        )
      (enumerate-interval 1 (length sudoku-solution))
      )))

     (define row-elements (list-ref sudoku-solution row-position))
     (if (or
          (check-list-element box-elements newValue)
          (check-list-element row-elements newValue)
          (check-list-element column-elements newValue)
          )
         '#f
         '#t
         )     
     )

  (define (get-next-move newposition prevIndex)
   

    (define row-position (car newposition))
    (define column-position (cadr newposition))

    (define row-count 0)

    (define position-possiblemoves null)
      (map
       (lambda(x)
         (cond
           ((eq? row-count row-position)
            (define column-count 0)
            (map
             (lambda(y)
               (cond
                 ((eq? column-count column-position)
                  (set! position-possiblemoves (list-ref (list-ref all-possiblemoves row-position) column-position))
                  (set! column-count (+ column-count 1))
                  )
                 (else
                  (set! column-count (+ column-count 1))
                  )
                 )
               )
             x)
            (set! row-count (+ row-count 1))
            )
           (else
            (set! row-count (+ row-count 1))
            )
           )
         )
       all-possiblemoves)
    
    (define next-index '#f)

    (cond
      ((eq? prevIndex 0) (set! next-index (list-ref position-possiblemoves 0)))
      (else
       (cond
        ((index-of position-possiblemoves prevIndex)
         (define new-index (+ (index-of position-possiblemoves prevIndex) 1))
          (cond
            ((< new-index (length position-possiblemoves))
             (set! next-index (list-ref position-possiblemoves new-index))
             )
            )
         )
        )
       )
      )
    
    next-index

    )


  (define (nextMove CurrentPosition prevIndex)
     
     (define newStateIndex 0)
     (define newStateIndexFlag '#f)
     (define StartingIndex prevIndex)

     (define row-position (car CurrentPosition))
     (define column-position (cadr CurrentPosition))
     
     (define (nextMoveState)
       (cond ((< StartingIndex (+ maxMoves 1))                           
              (define newposition (find-next-empty-position row-position column-position))

              (define newStartingIndex (get-next-move newposition StartingIndex))

              (cond
                (newStartingIndex
                   (set! newStateIndex (list newposition newStartingIndex))              
              
        (cond  ((CheckLegality newStateIndex)                
                (set! newStateIndexFlag '#t))
               (else                
                (set! StartingIndex newStartingIndex)               
                (nextMoveState))
               )                 
                 )
                )           
              )
         )
       (cond (newStateIndexFlag
              (update-list-position newStateIndex)
              newStateIndex)
             (else '#f)
             )            
       )
     (nextMoveState)
      )


  (define (update-all-possible-moves newStateIndex)    
    (define row-position (car (car newStateIndex)))
    (define column-position (cadr (car newStateIndex)))    
    (define newValue (cadr newStateIndex))    

    (define row (length all-possiblemoves))
    (define column (length (car all-possiblemoves)))

    (define (update-column column)
      (define temporary-column-values null)
      (cond
        ((list? column)
         (map (lambda(y)
             (cond ((not (= y newValue))
                    (set! temporary-column-values (append-list temporary-column-values y))))
                         ) column)
         )
        (else
         (set! temporary-column-values column)
         )
        )
     
      temporary-column-values
      )
    
    (define (get-updated-column-value column-list row-count)      
      (define count 0)
      
      (cond
        ((eq? row-count row-position)
         (map
       (lambda(x)
         (cond ((eq? count column-position)
               
               (set! count (+ count 1))
               newValue
               )
                 (else
                  (set! count (+ count 1))
                  (update-column x)
                  ))
         )
       column-list)
         )
        (else
         (map (lambda(y)
                (cond ((eq? count column-position)
                       (set! count (+ count 1))
                       (update-column y)
                       )
                      (else
                       (set! count (+ count 1))
                       y)
                      )
                ) column-list)
         )
            )            
        )       

    (define (box-starting-index position-value)
       (cond
         ((or (= position-value 0) (= position-value 1) (= position-value 2))
          0
          )
         ((or (= position-value 3) (= position-value 4) (= position-value 5))
          3
          )
         ((or (= position-value 6) (= position-value 7) (= position-value 8))
          6
          )         
         )
       )

     (define box-starting-row-position (box-starting-index row-position))
     (define box-starting-column-position (box-starting-index column-position))

    (define (box-updated-column-value column-list row-count)      
      (define column-count 0)
      (cond
        ((or (= box-starting-row-position row-count)
             (= (+ box-starting-row-position 1) row-count)
             (= (+ box-starting-row-position 2) row-count)
             )

         (map
          (lambda(y)
            (cond
           ((or (= box-starting-column-position column-count)
                (= (+ box-starting-column-position 1) column-count)
                (= (+ box-starting-column-position 2) column-count)
                )
            (set! column-count (+ column-count 1))
            (update-column y)
            )
           (else
            (set! column-count (+ column-count 1))
            y
            )
           )
            )
           column-list)        

         )
        (else column-list)
        )           
        )    

    (define row-count -1) 
     
    (set! all-possiblemoves (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (get-updated-column-value x row-count)               
               )
                 
            all-possiblemoves))
    ;update box values
    (set! row-count -1)
    (set! all-possiblemoves (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (box-updated-column-value x row-count)               
               )
                 
            all-possiblemoves))    
    )

  (define (get-non-empty-position i j)    
    
    (define (next-i-j-values)
      (cond ((and (< i 0) (< j 0))
             (set! i 0)
             (set! j 0)
             )
        ((and (< i 9) (< j 8))
             (set! j (+ j 1)))
            ((and (< i 8) (= j 8))
             (set! i (+ i 1))
             (set! j 0))
            ((and (= i 8) (= j 8))
             (set! i (+ i 1))
             (set! j (+ j 1))
             )
            )
      )

    (next-i-j-values)
    
    (define (find-i-and-jth-value)
      
      (cond
        ((and (= i 8) (= j 8))
         (cond ((not (eq? (list-ref (list-ref sudoku-solution i) j) 'nil))
                 (list (list i j) (list-ref (list-ref sudoku-solution i) j)))
                 (else
                  '#f)
                 ))
            ((and (< i 9) (< j 9))
             (cond ((not (eq? (list-ref (list-ref sudoku-solution i) j) 'nil))
                 (list (list i j) (list-ref (list-ref sudoku-solution i) j)) )
                 (else
                  (next-i-j-values)
                 (find-i-and-jth-value))
                 )
             )            
            (else '#f)
            )      
      )
    (find-i-and-jth-value)
    )

  (define (get-unit-length-list-position i j)    
    
    (define (next-i-j-values)
      (cond ((and (< i 9) (< j 8))
             (set! j (+ j 1)))
            ((and (< i 8) (= j 8))
             (set! i (+ i 1))
             (set! j 0))
            ((and (= i 8) (= j 8))
             (set! i (+ i 1))
             (set! j (+ j 1))
             )
            )
      )

    (cond
      ((and (or (> i 0) (> j 0))) (next-i-j-values)))
    
    (define (find-i-and-jth-value)
      (cond
        ((and (= i 8) (= j 8))
         (cond
           ((list? (list-ref (list-ref all-possiblemoves i) j))
            (if (= (length (list-ref (list-ref all-possiblemoves i) j)) 1)
                (list (list i j) (car (list-ref (list-ref all-possiblemoves i) j)))
                '#f
                )
            )
           
           (else '#f)
           )                      
           )         
            ((and (< i 9) (< j 9))
             
             (cond
           ((list? (list-ref (list-ref all-possiblemoves i) j))
            (cond ((= (length (list-ref (list-ref all-possiblemoves i) j)) 1)
                (list (list i j) (car (list-ref (list-ref all-possiblemoves i) j))))
                  (else
                   (next-i-j-values)
                   (find-i-and-jth-value)
                   )
                )
            )           
           (else
            (next-i-j-values)
            (find-i-and-jth-value))
           )
             )            
            (else '#f)
            )      
      )
    (find-i-and-jth-value)
    )
  
  ;constraint propagation
  (define (ConstraintPropagation)
    (define row -1)
    (define column -1)
    
    (define (update-all-possible-moves-with-solutionvalues)     
      
      (define nonempty-position (get-non-empty-position row column))
      (display "return from get non empty position")

      (cond
        (nonempty-position
         (set! row (car (car nonempty-position)))
         (set! column (cadr (car nonempty-position)))

         (update-all-possible-moves nonempty-position)
         (update-all-possible-moves-with-solutionvalues)
         )
        )
      )

    (define (update-sudoku-solution-with-single-possible-move)
      (define unit-length-element (get-unit-length-list-position row column))

      (cond
        (unit-length-element

         (update-list-position unit-length-element)
         (update-all-possible-moves unit-length-element)

         (update-sudoku-solution-with-single-possible-move)
         )
        )  
      )  
    
    (update-all-possible-moves-with-solutionvalues)  

    (set! row 0)
    (set! column 0)
    
    (update-sudoku-solution-with-single-possible-move)
    )

  ;next min length element
  (define (next-min-length-element row-value column-value)
      ;(display "\nInside minimun length element") (newline)     

      (define row-position row-value)
      (define column-position column-value)
      (define max-length 9)
      (define element-position null)
      (define list-elements null)
      (define new-column-position null)

      (define (next-row-column-values)
      (cond ((and (< row-position 0) (< column-position 0))
             (set! row-position 0)
             (set! column-position 0)
             )
        ((and (< row-position 9) (< column-position 8))
             (set! column-position (+ column-position 1)))
            ((and (< row-position 8) (= column-position 8))
             (set! row-position (+ row-position 1))
             (set! column-position 0))
            ((and (= row-position 8) (= column-position 8))
             (set! row-position (+ row-position 1))
             (set! column-position (+ column-position 1))
             )
            )
      )

      (next-row-column-values)
    
      (define (get-min-length-element list2)
        (set! list-elements list2)
        (define pos-count 0)
        (map
         (lambda(x)
          
           (cond                          
             ((and (list? x)(null? new-column-position) )
              
              (cond
               ((> pos-count (- column-position 1))
                
                (cond
                ((< (length x) 3)
                 
                 (set! new-column-position pos-count)
                 (set! pos-count (+ pos-count 1))                 
                 )
                (else
                 (set! pos-count (+ pos-count 1))
                 )
                )                
                )
               (else
                (set! pos-count (+ pos-count 1))
                )
               )              
              )
             (else
              (set! pos-count (+ pos-count 1))
              )             
             )
           )
         list2)        
        )     

      (define (element-in-row)
        (get-min-length-element (list-ref all-possiblemoves row-position))       
        (cond
          ((null? new-column-position)
           (cond
             ((and (= row-position 8) (= column-position 8))
              '#f
              )
             (else
              (set! row-position (+ row-position 1))
              (set! column-position 0)
              (element-in-row)              
              )
             )           
           )
          (else
           (list-ref list-elements new-column-position)
           )
        )            
        )
      (if (and (> row-position 8) (> column-position 8) )
          '#f
          (list (element-in-row) (list row-position new-column-position))
       )
      
      )

  ;check same in row 
  (define (same-in-row passed-element)
      ;(display "\nInside same in row") (newline)

      (define row (caadr passed-element))
  
      (define column (cadadr passed-element))
 
      (define row-elements (list-ref all-possiblemoves row))
   
      (define (check-rowlist-element list1 element)
        ;(display "\nInside check row list element: ")(newline)       
        
        (define rowlist-count 0)
        (define (check list1)
          (cond
            ((null? list1) '#f)
            ((= rowlist-count column)             
             (set! rowlist-count (+ rowlist-count 1))
             (check (cdr list1))
             )
            ((equal? (car list1) element)            
             (list (car list1) (list row rowlist-count)))
            (else             
             (set! rowlist-count (+ rowlist-count 1))
             (check (cdr list1)))
           ))
        (check list1)
        )

     
      (check-rowlist-element row-elements (car passed-element))     
    )

  ;check same in column
  (define (same-in-column passed-element)
      (display "\nInside same in column") (newline)
      
      (define row (caadr passed-element))
     
      (define column (cadadr passed-element))
     
      (define column-elements null)
        (map
         (lambda(x)
           (define column-count 0)
           (map
            (lambda(y)
              (cond
                ((= column-count column)
                 (set! column-elements (append-list column-elements y))
                 (set! column-count (+ column-count 1))
                 )
                (else
                 (set! column-count (+ column-count 1))
                 )
                )
              )
            x)
           )
         all-possiblemoves)   

      (define (check-columnlist-element list1 element)
        (define columnlist-count 0)
        (define (check list1)
          (cond
            ((null? list1) '#f)
            ((= columnlist-count row)
             (set! columnlist-count (+ columnlist-count 1))
             (check (cdr list1))
             )
            ((equal? (car list1) element)
             (list (car list1) (list row columnlist-count))
             )
            (else
             (set! columnlist-count (+ columnlist-count 1))
             (check (cdr list1)))
           ))
        (check list1)
        )
     
      (check-columnlist-element column-elements (car passed-element))
  )

  

  ;check same in box
  (define (same-in-box passed-element)
      (display "\nInside same in box") (newline)    

      (define row (caadr passed-element))
     
      (define column (cadadr passed-element))     

      (define (box-starting-index position-value)
       (cond
         ((or (= position-value 0) (= position-value 1) (= position-value 2))
          0
          )
         ((or (= position-value 3) (= position-value 4) (= position-value 5))
          3
          )
         ((or (= position-value 6) (= position-value 7) (= position-value 8))
          6
          )         
         )
       )

     (define box-starting-row-position (box-starting-index row))
     (define box-starting-column-position (box-starting-index column))

      (define box-elements null)
      (define box-elements-positions null)
      (define given-element-position null)

      (define (box-updated-column-value column-list row-count)      
      (define column-count 0)
      (define position-count 0)
      (cond
        ((or (= box-starting-row-position row-count)
             (= (+ box-starting-row-position 1) row-count)
             (= (+ box-starting-row-position 2) row-count)
             )
         (map
          (lambda(y)
            (cond
           ((or (= box-starting-column-position column-count)
                (= (+ box-starting-column-position 1) column-count)
                (= (+ box-starting-column-position 2) column-count)
                )          
            (cond
              ((eq? (car passed-element) y) (set! given-element-position position-count))
              )            
            (set! column-count (+ column-count 1))
            (set! position-count (+ position-count 1))
            (set! box-elements (append-list box-elements y))
            (set! box-elements-positions (append-list box-elements-positions (list row-count column-count)))
            )
           (else
            (set! column-count (+ column-count 1))
            
            )
           )
            )
           column-list)
         
         )
        (else column-list)
        )           
        )

      (define row-count -1)
      (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (box-updated-column-value x row-count)               
               )
                 
            all-possiblemoves)
    
      (define (check-boxlist-element list1 element)
        (define columnlist-count 0)
        (define (check list1)
          (cond
            ((null? list1) '#f)
            ((= columnlist-count given-element-position)
             (set! columnlist-count (+ columnlist-count 1))
             (check (cdr list1))
             )
            ((eq? (car list1) element) '#t)
            (else
             (set! columnlist-count (+ columnlist-count 1))
             (check (cdr list1)))
           ))
        (check list1)
        )
     
      (check-boxlist-element box-elements (car passed-element))
      
      )

  ;operation if the element is same with another element
    (define (element-operation curr-element same-element)
      (display "\nInside element operation")(newline)
      (display "passed curr-element values: ")
      (display curr-element) (newline)
      (display "passed same-element values: ")
      (display same-element) (newline)
      )

  ;domain constraint propagation
  (define (domain-constriant-propagation)
    (display "\nInside domain constraint propagation")(newline)
    
    (display "all possible moves after constraint propagation: ") (newline)
    (display all-possiblemoves) (newline)

    ;domain constraint propagation operation
    (define (propagation-operation)      
      (set! curr-element (next-min-length-element initial-row initial-column))
      (display "\nInside propagation operation") (newline)
      (display "initial row: ")
      (display initial-row) (newline)
      (display "initial column: ")
      (display initial-column) (newline)
      
      (display "curr element: ")
      (display curr-element) (newline) (newline)
      (define same-element null)      
      (cond
        (curr-element
        
         (set! initial-row (caadr curr-element))
         (set! initial-column (cadadr curr-element))
        
         (cond
           ((same-in-row curr-element)
            (display "passed same in row condition: ") (newline)
            (set! same-element (same-in-row curr-element))
            (display "returned value from same in row: ")
            (display same-element) (newline) (newline)
            (element-operation curr-element same-element)            
            )
           ((same-in-column curr-element)
            (set! same-element (same-in-column curr-element))
            (element-operation curr-element same-element)            
            )
           ((same-in-box curr-element)
            (set! same-element (same-in-box curr-element))
            (element-operation curr-element same-element)
            
            )           
           )        
         ;(propagation-operation)
         )
        (else '#f)
        )      
      )
   
    ;(define initial-row -1)
    ;(define initial-column -1)

    (define initial-row 0)
    (define initial-column 8)  

    (define curr-element null)
    (propagation-operation)    
    )
  
  (define (SolveSudokuProblem)    
     (DepthFirstSearch Path nextMove checkFinalState MaxDepth update-list-position InitialIndex)     
    )
  
   (define sudoku-solution sudoku-problem)   
   (define Possiblemoves (list 1 2 3 4 5 6 7 8 9))

  (define all-possiblemoves
    (map
     (lambda(x)
       (map
        (lambda(y) Possiblemoves)
        (enumerate-interval 1 (length (car sudoku-solution))))
       )
     (enumerate-interval 1 (length sudoku-solution)))
    )  
  
   (define maxMoves 9)
   (define InitialIndex 0)
  
  (define InitialState (list (list 0 0) InitialIndex))
   (define Path (list InitialState))
   (define MaxDepth 1000)

  (ConstraintPropagation)

  (display "\nAfter constraint propagation") (newline)
  (display "value of all possible moves: ")
  (display all-possiblemoves) (newline) (newline)
  (display "value of sudoku solution: ")
  (display sudoku-solution) (newline)

  (define test-newstateIndex (list (list 6 0) (list 0 0)))
  (update-all-possible-moves test-newstateIndex)

  (display "value of all possible moves after update: ")
  (display all-possiblemoves) (newline) (newline)

  

  ;(domain-constriant-propagation)

  ;(cond
    ;((not (find-next-empty-position 0 0))
    ; (display "Solution found only by constraint propagation")(newline)
    ; (display "\nSolution: ")(newline)
    ; (display sudoku-solution)
     
   ; )
   ; (else
    ; (display "Solution found through constraint propagation and Iterative-DFS")(newline)
     ;(display "Remaining solution after constraint propagation: ") (newline)
    ; (display sudoku-solution) (newline)
     
    ; (display "\nSolution: ")(newline)
     ;(display (SolveSudokuProblem))
    ; )
    ;) 
 )



(define sudoku-problem-world-hardest
  ;world hardest problem
  (list (list  8   'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil)
        (list 'nil 'nil  3    6   'nil 'nil 'nil 'nil 'nil)
        (list 'nil  7   'nil 'nil  9   'nil  2   'nil 'nil)
        (list 'nil  5   'nil 'nil 'nil  7   'nil 'nil 'nil)
        (list 'nil 'nil 'nil 'nil  4    5    7   'nil 'nil)
        (list 'nil 'nil 'nil  1   'nil 'nil 'nil  3   'nil)
        (list 'nil 'nil  1   'nil 'nil 'nil 'nil  6    8)
        (list 'nil 'nil  8    5   'nil 'nil 'nil  1   'nil)
        (list 'nil  9   'nil 'nil 'nil 'nil  4   'nil 'nil)))

(define sudoku-problem-easiest
  (list (list 'nil 'nil 6 'nil 7 8 4 9 'nil)
        (list 5 'nil 9 1 3 'nil 7 6 8)
        (list 4 'nil 7 6 'nil 9 5 3 'nil)
        (list 2 'nil 3 4 1 5 9 8 7)
        (list 9 'nil 4 8 6 'nil 1 2 5)
        (list 8 'nil 1 7 'nil 2 6 4 'nil)
        (list 1 'nil 8 9 4 7 2 5 6)
        (list 6 'nil 'nil 3 5 1 8 7 4)
        (list 'nil 'nil 5 2 8 6 3 'nil 9)))

(define sudoku-problem-easy
  (list (list  9   'nil 'nil 1    3    'nil 'nil 8    'nil)
        (list 'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil 7)
        (list 8    'nil 'nil 7    6    4    9    1    2)
        (list 6    'nil 'nil 'nil 'nil 9    1    'nil 8)
        (list 5    'nil 3    8    7    'nil 6    'nil 9)
        (list 'nil 'nil 'nil 'nil 5    'nil 'nil 7    4)
        (list 'nil 1    9    4    'nil 'nil 'nil 'nil 5)
        (list 'nil 'nil 'nil 9    'nil 'nil 2    4    3)
        (list 'nil 'nil 2    6    8    3    7    'nil 'nil)))

(define sudoku-problem-medium
  (list (list 6    5    'nil 'nil 'nil 'nil 4    7    9)
        (list 'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil 8)
        (list 7    'nil 3    'nil 'nil 'nil 'nil 'nil 'nil)
        (list 'nil 1    'nil 'nil 4    3    'nil 2    6)
        (list 9    'nil 'nil 1    7    'nil 'nil 4    'nil)
        (list 'nil 3    'nil 6    'nil 'nil 'nil 'nil 'nil)
        (list 5    'nil 'nil 'nil 2    4    'nil 'nil 3)
        (list 'nil 'nil 4    'nil 1    'nil 6    'nil 'nil)
        (list 1    7    9    3    'nil 'nil 'nil 'nil 'nil)))

(define sudoku-problem-hard
  (list (list 'nil 4    'nil 9    'nil 'nil 'nil 2    'nil)
        (list 'nil 1    'nil 'nil 'nil 7    9    'nil 'nil)
        (list 'nil 'nil 'nil 3    2    'nil 'nil 'nil 'nil)
        (list 8    'nil 'nil 'nil 6    3    'nil 7    'nil)
        (list 3    'nil 'nil 'nil 'nil 'nil 2    'nil 'nil)
        (list 6    'nil 'nil 'nil 'nil 4    5    'nil 'nil)
        (list 4    'nil 'nil 'nil 'nil 9    'nil 'nil 8)
        (list 'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil)
        (list 'nil 5    'nil 'nil 'nil 6    7    'nil 'nil)))

(define sudoku-problem-expert
  (list (list 9    'nil 'nil 'nil 2    'nil 'nil 'nil 'nil)
        (list 'nil 8    'nil 'nil 1    'nil 'nil 'nil 4)
        (list 'nil 'nil 'nil 'nil 3    'nil 'nil 'nil 'nil)
        (list 'nil 'nil 4    7    'nil 'nil 1    3    'nil)
        (list 'nil 2    'nil 'nil 'nil 'nil 'nil 'nil 6)
        (list 'nil 'nil 'nil 4    'nil 9    'nil 'nil 8)
        (list 7    3    'nil 'nil 'nil 'nil 2    6    'nil)
        (list 'nil 'nil 'nil 'nil 'nil 3    'nil 'nil 'nil)
        (list 6    'nil 1    'nil 'nil 'nil 4    'nil 'nil)))

(define sudoku-problem-evil
  (list (list 'nil 7    'nil 'nil 'nil 'nil 'nil 8    'nil)
        (list 3    'nil 'nil 'nil 5    9    6    'nil 'nil)
        (list 'nil 'nil 'nil 4    'nil 'nil 'nil 'nil 'nil)
        (list 'nil 'nil 'nil 2    'nil 'nil 1    'nil 'nil)
        (list 4    'nil 'nil 6    'nil 'nil 'nil 'nil 'nil)
        (list 'nil 3    'nil 'nil 4    1    'nil 'nil 7)
        (list 'nil 'nil 'nil 'nil 'nil 2    'nil 'nil 'nil)
        (list 5    'nil 'nil 'nil 1    3    9    'nil 'nil)
        (list 'nil 'nil 9    'nil 'nil 'nil 'nil 'nil 6)))


;(display "\nEasiest sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-easiest)(newline)

;(display "\nEasy sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-easy)(newline)

(display "\nMedium sudoku problem solution: ")(newline)
(sudoku-solver sudoku-problem-medium)(newline)

;(display "\nHard sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-hard)(newline)

;(display "\nExpert sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-expert)(newline)

;(display "\nEvil sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-evil)(newline)

;(display "\nWorlds Hardest sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-world-hardest)(newline)