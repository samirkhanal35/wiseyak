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
      (cond ((and (< i maxMoves) (< j (- maxMoves 1)))
             (set! j (+ j 1)))
            ((and (< i (- maxMoves 1)) (= j (- maxMoves 1)))
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
        ((and (= i (- maxMoves 1)) (= j (- maxMoves 1)))
         (cond ((eq? (list-ref (list-ref sudoku-solution i) j) 'nil)
                 (list i j) )
                 (else
                  '#f)
                 ))
            ((or (< i maxMoves) (< j maxMoves))
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

   
    
    
     (define (get-starting-row-position x)
      (* (quotient x box-size) box-size)
      )

    (define (get-starting-column-position x)
      (* (remainder x box-size) box-size)
      )
    
     (define box-starting-row-position (get-starting-row-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))
     (define box-starting-column-position (get-starting-column-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))

   
     (define box-elements (fringe (map
      (lambda(x)
        (map
         (lambda(y) (list-ref (list-ref sudoku-solution  x) y))
         (enumerate-interval box-starting-column-position (+ box-starting-column-position (- box-size 1)))
         )
        )
      (enumerate-interval box-starting-row-position (+ box-starting-row-position (- box-size 1)))
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
                  (set! position-possiblemoves (car (list-ref (list-ref all-possiblestates row-position) column-position)))
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
       all-possiblestates)

   
    
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
              ;(display "solution path: ")
              ;(display Path) (newline)
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
    

    (define row (length all-possiblestates))
    (define column (length (car all-possiblestates)))

    (define (update-column column)
      
      (define temporary-column-values null)
      (cond
        ((list? (car column))
         (map (lambda(y)
             (cond ((not (= y newValue))
                    (set! temporary-column-values (append-list temporary-column-values y))))
                         ) (car column))
         )
        (else
         (set! temporary-column-values (car column))
         )
        )
    
     
      (list temporary-column-values (cadr column) (caddr column) (cadddr column))
      )
    
    (define (get-updated-column-value column-list row-count)      
      (define count 0)
      
      (cond
        ((eq? row-count row-position)
         (map
       (lambda(x)
         (cond ((eq? count column-position)
               
               (set! count (+ count 1))
               (list newValue (cadr x) (caddr x) (cadddr x))
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

    (define (get-starting-row-position x)
      (* (quotient x box-size) box-size)
      )

    (define (get-starting-column-position x)
      (* (remainder x box-size) box-size)
      )
    
     (define box-starting-row-position (get-starting-row-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))
     (define box-starting-column-position (get-starting-column-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))

    (define (box-updated-column-value column-list row-count)
    
      (define column-count 0)       
      (cond
        ( (check-list-element (enumerate-interval box-starting-row-position (+ box-starting-row-position (- box-size 1))) row-count)
        
          (map
          (lambda(y)
            (cond
           ((check-list-element (enumerate-interval box-starting-column-position (+ box-starting-column-position (- box-size 1))) column-count)
           
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
     
    (set! all-possiblestates (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (get-updated-column-value x row-count)               
               )
                 
            all-possiblestates))

   
    ;update box values
    (set! row-count -1)
    (set! all-possiblestates (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (box-updated-column-value x row-count)               
               )
                 
            all-possiblestates))    
    )

  (define (get-non-empty-position i j)    
    
    (define (next-i-j-values)
      (cond ((and (< i 0) (< j 0))
             (set! i 0)
             (set! j 0)
             )
        ((and (< i maxMoves) (< j (- maxMoves 1)))
             (set! j (+ j 1)))
            ((and (< i (- maxMoves 1)) (= j (- maxMoves 1)))
             (set! i (+ i 1))
             (set! j 0))
            ((and (= i (- maxMoves 1)) (= j (- maxMoves 1)))
             (set! i (+ i 1))
             (set! j (+ j 1))
             )
            )
      )

    (next-i-j-values)
    
    (define (find-i-and-jth-value)
      
      (cond
        ((and (= i (- maxMoves 1)) (= j (- maxMoves 1)))
         (cond ((not (eq? (list-ref (list-ref sudoku-solution i) j) 'nil))
                 (list (list i j) (list-ref (list-ref sudoku-solution i) j)))
                 (else
                  '#f)
                 ))
            ((and (< i maxMoves) (< j maxMoves))
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
      (cond ((and (< i maxMoves) (< j (- maxMoves 1)))
             (set! j (+ j 1)))
            ((and (< i (- maxMoves 1)) (= j (- maxMoves 1)))
             (set! i (+ i 1))
             (set! j 0))
            ((and (= i (- maxMoves 1)) (= j (- maxMoves 1)))
             (set! i (+ i 1))
             (set! j (+ j 1))
             )
            )
      )
    
    (cond
      ((and (or (> i 0) (> j 0))) (next-i-j-values)))
    
    (define (find-i-and-jth-value)
      (cond
        ((and (= i (- maxMoves 1)) (= j (- maxMoves 1)))
         (cond
           ((list? (car (list-ref (list-ref all-possiblestates i) j)))
            (if (= (length (car (list-ref (list-ref all-possiblestates i) j))) 1)
                (list (list i j) (caar (list-ref (list-ref all-possiblestates i) j)))
                '#f
                )
            )
           
           (else '#f)
           )                      
           )         
            ((and (< i maxMoves) (< j maxMoves))
             
             (cond
           ((list? (car (list-ref (list-ref all-possiblestates i) j)))
            (cond ((= (length (car (list-ref (list-ref all-possiblestates i) j))) 1)
                (list (list i j) (caar (list-ref (list-ref all-possiblestates i) j))))
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

  (define (update-sudoku-solution-with-single-possible-move)
      ;(display "\nInside update solution with single possible move") (newline)
      (define row 0)
      (define column 0)
    
      (define unit-length-element (get-unit-length-list-position row column))
    
      (cond
        (unit-length-element

         (update-list-position unit-length-element)
         
         (update-all-possible-moves unit-length-element)

         (update-sudoku-solution-with-single-possible-move)
         )
        )  
      )
  
  (define (ConstraintPropagation)
    (define row -1)
    (define column -1)
    
    (define (update-all-possible-moves-with-solutionvalues)     
      
      (define nonempty-position (get-non-empty-position row column))

      (cond
        (nonempty-position
         (set! row (car (car nonempty-position)))
         (set! column (cadr (car nonempty-position)))

         (update-all-possible-moves nonempty-position)
         (update-all-possible-moves-with-solutionvalues)
         )
        )
      )     
    
    (update-all-possible-moves-with-solutionvalues)
   
    (update-sudoku-solution-with-single-possible-move)
    )

  (define (get-next-list-element i j)    
    
    (define (next-i-j-values)
      (cond ((and (< i 0) (< j 0))
             (set! i 0)
             (set! j 0)
             )
        ((and (< i maxMoves) (< j (- maxMoves 1)))
             (set! j (+ j 1)))
            ((and (< i (- maxMoves 1)) (= j (- maxMoves 1)))
             (set! i (+ i 1))
             (set! j 0))
            ((and (= i (- maxMoves 1)) (= j (- maxMoves 1)))
             (set! i (+ i 1))
             (set! j (+ j 1))
             )
            )
      )

    (next-i-j-values)
    
    (define (find-i-and-jth-value)
      (cond
        ((and (= i (- maxMoves 1)) (= j (- maxMoves 1)))
         (cond
           ((list? (car (list-ref (list-ref all-possiblestates i) j)))
            (list-ref (list-ref all-possiblestates i) j)
            )
           
           (else '#f)
           )                      
           )         
            ((and (< i maxMoves) (< j maxMoves))
             
             (cond
           ((list? (car (list-ref (list-ref all-possiblestates i) j)))
            (list-ref (list-ref all-possiblestates i) j)
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

  (define (get-unique-elements prev-elements curr-elements)
    (display "\nInside get unique elements") (newline)
    (display "passed prev-elements: ")
    (display prev-elements) (newline)
    (display "passed current elements: ")
    (display curr-elements) (newline)
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
       

    ;(define arr1 (list 1))
    ;(display (cadr arr1))(newline)
        new-unique-elements
        )

  (define (get-box-elements box)
      (display "\nInside get box elements") (newline)
      (display "box value: ")
      (display box) (newline)
      (define box-elements null)
      (map
       (lambda(x)
         (map
          (lambda(y)
            (cond
              ((eq? box (cadr y))
               (set! box-elements (append-list box-elements y))
               )
              )
            )
          x)
         )
       all-possiblestates)
      box-elements
      )

  (define (get-column-elements column)
      (define column-elements null)
      (define column-count 0)
      (map
       (lambda(x)
         (set! column-count 0)
         (map
          (lambda(y)
            (cond
              ((eq? column-count column)
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
       all-possiblestates
      )
      column-elements
    )

  (define (update-all-possiblestates-box unique-elements temp-states)
    (display "\nInside update all possiblestates box values") (newline)
    (display "passed temp-states: ")
    (display temp-states) (newline)
    (display "passed unique elements: ")
    (display unique-elements) (newline)

    (define (update-all-possible-moves)
    
    (define row (length all-possiblestates))
    (define column (length (car all-possiblestates)))

    (define (update-column column)
      (define temporary-column-values null)
      (cond
        ((list? (car column))
         (map (lambda(y)
             (cond ((not (check-list-element unique-elements y))
                    (set! temporary-column-values (append-list temporary-column-values y))))
                         ) (car column))
         )
        (else
         (set! temporary-column-values (car column))
         )
        )
      (list temporary-column-values (cadr column) (caddr column) (cadddr column))
      )              

    (define (get-starting-row-position x)
      (* (quotient x box-size) box-size)
      )

    (define (get-starting-column-position x)
      (* (remainder x box-size) box-size)
      )
    
     (define box-starting-row-position (get-starting-row-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))
     (define box-starting-column-position (get-starting-column-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))

    (define (box-updated-column-value column-list row-count)
    
      (define column-count 0)       
      (cond
        ( (check-list-element (enumerate-interval box-starting-row-position (+ box-starting-row-position (- box-size 1))) row-count)        
          (map
          (lambda(y)
            (cond
           ((check-list-element (enumerate-interval box-starting-column-position (+ box-starting-column-position (- box-size 1))) column-count)
            (cond
              ((and (check-list-element row-values (caddr y)) (check-list-element column-values (cadddr y)))
               (display "inside and condition") (newline)
               (display "value of y: ")
               (display y) (newline)
               (set! column-count (+ column-count 1))
               y
               )
              (else
               (set! column-count (+ column-count 1))
               (update-column y)
               )
              )
            
            )
           (else
            (set! column-count (+ column-count 1))
            y
            )
           )
            )
           column-list)    
         )
        (else
         column-list)
        )           
        )
      
    ;update box values
    (define row-count -1)
    (set! all-possiblestates (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (box-updated-column-value x row-count)               
               )
                 
            all-possiblestates))

    (display "before updating all possiblestates with single possible move: ")
    (display all-possiblestates) (newline)
      
    (update-sudoku-solution-with-single-possible-move)
    )


    (define row-position (caddr (car temp-states)))
    (define column-position (cadddr (car temp-states)))

    (define row-values
      (map
       (lambda(x)
         (caddr x))
       temp-states)
      )
    
    (define column-values
      (map
       (lambda(x)
         (cadddr x))
       temp-states)
      )

    (display "\nrow values: ")
    (display row-values) (newline)

    (display "column values: ")
    (display column-values) (newline)

    (display "before updating all possiblestates: ")
    (display all-possiblestates) (newline)
    
    (update-all-possible-moves)

    (display "after updating all possiblestates: ")
    (display all-possiblestates) (newline)
    
    ;(define breakpoint (list 1))
    ;(display (cadr breakpoint))(newline)       
    )

  (define (update-all-possiblestates-row unique-elements temp-states)
    (display "\nInside update all possiblestates row values") (newline)
    (display "passed temp-states: ")
    (display temp-states) (newline)
    (display "passed unique elements: ")
    (display unique-elements) (newline)


    (define (update-all-possible-moves)
   
    (define row (length all-possiblestates))
    (define column (length (car all-possiblestates)))

    (define (update-column column)
      
      (define temporary-column-values null)
      (cond
        ((list? (car column))
         (map (lambda(y)
             (cond ((not (check-list-element unique-elements y))
                    (set! temporary-column-values (append-list temporary-column-values y))))
                         ) (car column))
         )
        (else
         (set! temporary-column-values (car column))
         )
        )
    
     
      (list temporary-column-values (cadr column) (caddr column) (cadddr column))
      )
    
    (define (get-updated-column-value column-list row-count)      
      (define count 0)
      
      (cond
        ((eq? row-count row-position)
         (map
       (lambda(x)
         (cond ((check-list-element column-values (cadddr x))                
               (set! count (+ count 1))
               x
               )
               (else
                  (set! count (+ count 1))
                  (update-column x)
                  )
               )
         )
       column-list)
         )
        (else
         column-list
         )
            )
            
        )               

    (define row-count -1) 
     
    (set! all-possiblestates (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (get-updated-column-value x row-count)               
               )
                 
            all-possiblestates))

      (update-sudoku-solution-with-single-possible-move)
      
    )

    (define row-position (caddr (car temp-states)))
    (display "row value: ")
    (display row-position) (newline)

    (define column-values
      (map
       (lambda(x)
         (cadddr x))
       temp-states)
      )
    (display "column values: ")
    (display column-values) (newline)

    (display "before updating all possiblestates: ")
    (display all-possiblestates) (newline)

    
    (update-all-possible-moves)

    (display "after updating all possiblestates: ")
    (display all-possiblestates) (newline)

    ;(define breakpoint (list 1))
    ;(display (cadr breakpoint))(newline)    
    )

  (define (update-all-possiblestates-column unique-elements temp-states)
    (display "\nInside update all possiblestates column values") (newline)
    (display "passed temp-states: ")
    (display temp-states) (newline)
    (display "passed unique elements: ")
    (display unique-elements) (newline)

    (define (update-all-possible-moves)      

    (define row (length all-possiblestates))
    (define column (length (car all-possiblestates)))

    (define (update-column column)
      
      (define temporary-column-values null)
      (cond
        ((list? (car column))
         (map (lambda(y)
             (cond ((not (check-list-element unique-elements y))
                    (set! temporary-column-values (append-list temporary-column-values y))))
                         ) (car column))
         )
        (else
         (set! temporary-column-values (car column))
         )
        )
    
     
      (list temporary-column-values (cadr column) (caddr column) (cadddr column))
      )
      
    
    (define (get-updated-column-value column-list row-count)      
      (define count 0)     
         (map (lambda(y)
                (cond ((eq? count column-position)
                       (cond
                         ((check-list-element row-values (caddr y))
                          (set! count (+ count 1))
                          y
                          )
                         (else
                          (set! count (+ count 1))
                          (update-column y)
                          )
                         )
                       
                       )
                      (else
                       (set! count (+ count 1))
                       y)
                      )
                ) column-list)         
        )               

    (define row-count -1) 
     
    (set! all-possiblestates (map (lambda(x)           
                  (set! row-count (+ row-count 1))
                (get-updated-column-value x row-count)               
               )
                 
            all-possiblestates))
      
   (update-sudoku-solution-with-single-possible-move)
    )

    (define column-position (cadddr (car temp-states)))
    (display "column value: ")
    (display column-position) (newline)

    (define row-values
      (map
       (lambda(x)
         (caddr x))
       temp-states)
      )
    (display "row values: ")
    (display row-values) (newline)

    (display "before updating all possiblestates: ")
    (display all-possiblestates) (newline)

    
    (update-all-possible-moves)

    (display "after updating all possiblestates: ")
    (display all-possiblestates) (newline)
    
    ;(define breakpoint (list 1))
    ;(display (cadr breakpoint))(newline)
    
    )

  (define (update-all-possiblestates-arc-consistancy temp-unique-elements temp-states flag)
    (display "\nInside update all possible states arc consistancy")(newline)
    (display "passed temp states: ")
    (display temp-states) (newline)

    (display "passed flag: ")
    (display flag) (newline)

    (cond
      ((eq? flag 1)
       (display "passed elements are of row")(newline)
       (update-all-possiblestates-row temp-unique-elements temp-states)
       )
      ((eq? flag 2)
       (display "passed elements are of column")(newline)
       (update-all-possiblestates-column temp-unique-elements temp-states)
       )
      ((eq? flag 3)
       (display "passed elements are of box")(newline)
       (update-all-possiblestates-box temp-unique-elements temp-states)
       )
      )

    ;(define breakpoint (list 1))
    ;(display (cadr breakpoint))(newline)
    
    )

  (define (arc-operation unique-list list-element all-elements flag)
    (display "\nInside arc-operation ")(newline)
    (display "passed unique-list: ")
    (display unique-list) (newline)
    (display "passed list element: ")
    (display list-element) (newline)
    (display "passed all-elements : ")
    (display all-elements) (newline)

    (define row (caddr list-element))
    (define column (cadddr list-element))

    (define list-only-elements null)

    (define (get-list-elements)
      (map
       (lambda(x)
         (cond
           ((eq? flag 1)
            (cond
              ((and (list? (car x)) (not(eq? column (cadddr x)))) 
              (set! list-only-elements (append-list list-only-elements x))
              )
             )
            )
          ((eq? flag 2)
            (cond
              ((and (list? (car x)) (not(eq? row (caddr x)))) 
              (set! list-only-elements (append-list list-only-elements x))
              )
             )
            )
          ((eq? flag 3)
            (cond
              ((and (list? (car x)) (not(and (eq? column (cadddr x)) (eq? row (caddr x))))) 
              (set! list-only-elements (append-list list-only-elements x))
              )
             )
            )
           )
         )
       all-elements)
      )

    (get-list-elements)

    (display "list only elements: ")
    (display list-only-elements) (newline)
    
    (define curr-element (car list-element))
    (display "current element: ")
    (display curr-element) (newline)

    (define unique-elements (car unique-list))
    (define states (cadr unique-list))    

    (define temp-unique-list unique-list)
    (define temp-unique-elements unique-elements)
    (define temp-states states)    

    (define (check-operation new-state)
      (display "\nInside check operation")(newline)
      (display "passed new-state: ")
      (display new-state) (newline)
      
      (display "return from get unique elements: ") 
      (set! temp-unique-elements (get-unique-elements unique-elements (car new-state)))
      (display temp-unique-elements) (newline)

      (set! temp-states (append-list temp-states new-state))
      (display "temp states: ")
      (display temp-states) (newline)

      (cond
        ((eq? (length temp-unique-elements) (length temp-states))
         (display "\nInside equal length of unique elements and no. of states")(newline)
         (update-all-possiblestates-arc-consistancy temp-unique-elements temp-states flag)
         '#t
         )
        (else
         '#f)
        )

      ;(define xx (list 1))
      ;(display (cadr xx))
                  
      )
    
    (display "row : ")
    (display row) (newline)

    (display "column : ")
    (display column) (newline)

    (define return-values (map
     (lambda(x)
       (set! temp-states states)
       (check-operation x)
       )
     list-only-elements))

    ;(cond
    ;  ((= flag 3)
    ;   (define return-values (map
    ; (lambda(x)
    ;   (set! temp-states states)
    ;   (check-operation x)
    ;   )
    ; list-only-elements))
    ;   (check-list-element return-values '#t)
    ;   )
    ;  (else
    ;   '#f)
    ;  )
    
    (check-list-element return-values '#t)
    
    
       
    )
  
  (define (propagation-process unique-list list-element)
    (display "\nInside propagation process")(newline)
    (display "passed unique list: ")
    (display unique-list) (newline)
    (display "passed list element: ")
    (display list-element) (newline)
    
    (define row (caddr list-element))
    (define column (cadddr list-element))
    (define box (cadr list-element))
    
    (display "row: ")
    (display row) (newline)
    (display "column: ")
    (display column)(newline)
    (display "box value: ")
    (display box) (newline)    

    (define row-elements (list-ref all-possiblestates row))
    
    (display "row elements: ")
    (display row-elements) (newline)

    (define column-elements (get-column-elements column))     

    (display "column elements: ")
    (display column-elements) (newline)

    (define box-elements (get-box-elements box))
    
    (display "box elements: ")
    (display box-elements) (newline)

    (cond
      ((arc-operation unique-list list-element row-elements 1)
       (display "arc-operation with row-elements passed") (newline)
       '#t)
      ((arc-operation unique-list list-element column-elements 2)
       (display "arc-operation with column-elements passed") (newline)
       '#t)
      ((arc-operation unique-list list-element box-elements 3)
       (display "arc-operation with box-elements passed") (newline)
       '#t)
      (else
       (display "arc-operation else condition") (newline)
       '#f)
      )
    )

  
  (define (arcConsistency)
    (display "\n Inside arc Consistency") (newline)
    (define row -1)
    (define column -1)

    (define (update-list-element list-element new-element)
      (display "\nInside update list element") (newline)
      (display "passed list-element: ")
      (display list-element) (newline)
      (display "\nnew element: ")
      (display new-element)(newline)     

      (cond
        ((null? list-element)
         (display "list element is empty")(newline)
         (list (car new-element) (list new-element) (list (cdr new-element)) )
         )
        (else
         (define unique-elements (car list-element))
         (define list-of-elements (cadr list-element))
         (define positions (caddr list-element))
         (set! unique-elements (get-unique-elements unique-elements (cdr new-element)))
         (set! list-of-elements (append-list list-of-elements new-element))
         (set! positions (append-list positions (caddr new-element)))
         (list unique-elements list-of-elements positions)
         )        
       )    
      )    

    (define (propagation)
      (define list-element (get-next-list-element row column) )
      ;(define list-element (list (list 2 6) 3 4 1))
      (display "\nList element: ")
      (display list-element) (newline)
      (define unique-list null)
      
      (cond
        (list-element
         (set! row (caddr list-element))
         (set! column (cadddr list-element))
         (display "updated row and column: ")
         (display row)
         (display column)(newline)
         (set! unique-list (update-list-element unique-list list-element))
         (display "return from update-list-element: ")
         (display unique-list) (newline)
         (propagation-process unique-list list-element)
         (propagation)
         )
        )      
      )
    
    (propagation)
    )
  
  ;Solve sudoku problem
  (define (SolveSudokuProblem)    
     (DepthFirstSearch Path nextMove checkFinalState MaxDepth update-list-position InitialIndex)     
    )
  
  (define sudoku-solution sudoku-problem)    

  (define maxMoves (length sudoku-problem))
  (define box-size (sqrt maxMoves))
  (define Possiblemoves (enumerate-interval 1 maxMoves))  
  
  (define (box-position row-position column-position)
    (define row-value (quotient row-position box-size))
    (define column-value (quotient column-position box-size))
    (+ (* row-value box-size) column-value)
    )

  (define row-count -1)
  (define column-count -1)
  (define all-possiblestates
    (map
     (lambda(x)
       (set! row-count (+ row-count 1))
       (set! column-count -1)
       (map
        (lambda(y)
          (set! column-count (+ column-count 1))
          (list Possiblemoves (box-position x y) row-count column-count))
        (enumerate-interval 0 (- (length (car sudoku-solution)) 1)))
       )
     (enumerate-interval 0 (- (length sudoku-solution) 1)))
    )
   
   (define InitialIndex 0)
  
  (define InitialState (list (list 0 0) InitialIndex))
  (define Path (list InitialState))
  (define MaxDepth (* maxMoves maxMoves))

  (display "all possible states: ")(newline)
  (display all-possiblestates) (newline)
  
  (ConstraintPropagation)

  (display "\nall possible states after constraint propagation: ")
  (display all-possiblestates) (newline)

  (arcConsistency)

  (display "\nall possible states after arc Consistency: ")
  (display all-possiblestates) (newline)


  (define final-solution null)
  (cond
    ((not (find-next-empty-position 0 0))
     (set! final-solution sudoku-solution)
     (display "Solution found only by constraint propagation")(newline)
     (display "\nFinal Solution: ")(newline)
     (display final-solution)     
    )
    (else
     (set! final-solution (SolveSudokuProblem))
     (display "Solution found through constraint propagation and Iterative-DFS")(newline)     
     (display "\nSolution Steps: ")(newline)
     (display final-solution)(newline)
     (display "\nFinal Solution: ") (newline)
     (display sudoku-solution) (newline)     
     )
    )
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

(define sudoku-solution-fourbyfour
  (list (list 2 11 9 5 8 16 13 4 12 3 14 7 10 6 15 1)
        (list 4 12 15 10 3 6 9 11 13 5 8 1 16 7 14 2)
        (list 1 14 6 7 15 2 5 12 11 9 10 16 3 13 8 4)
        (list 16 13 8 3 14 1 10 7 4 6 2 15 9 11 5 12)
        (list 12 2 16 9 10 14 15 13 8 1 5 3 6 4 11 7)
        (list 6 7 1 11 5 12 8 16 9 15 4 2 14 10 3 13)
        (list 14 5 4 13 6 11 1 3 16 12 7 10 8 9 2 15)
        (list 3 8 10 15 4 7 2 9 6 14 13 11 1 12 16 5)
        (list 13 9 2 16 7 8 14 10 3 4 15 6 12 5 1 11)
        (list 5 4 14 6 2 13 12 1 10 16 11 8 15 3 7 9)
        (list 7 1 11 12 16 4 3 15 5 13 9 14 2 8 10 6)
        (list 10 15 3 8 9 5 11 6 2 7 1 12 4 14 13 16)
        (list 11 10 13 14 1 9 7 8 15 2 6 4 5 16 12 3)
        (list 15 3 7 4 12 10 6 5 1 8 16 13 11 2 9 14)
        (list 8 6 5 1 13 3 16 2 14 11 12 9 7 15 4 10)
        (list 9 16 12 2 11 15 4 14 7 10 3 5 13 1 6 8)))

(define sudoku-problem-fourbyfour
  (list (list 'nil 11   9    'nil 'nil 16   13   4    'nil 'nil 14   'nil 10   6    15   'nil)
        (list 4    12   15   'nil 3    6    'nil 11   'nil 5    'nil 1    16   7    14   2)
        (list 1    'nil 6    'nil 15   2    'nil 'nil 11   9    10   'nil 'nil 'nil 8    'nil)
        (list 'nil 13   'nil 'nil 'nil 1    'nil 'nil 4    6    'nil 15   'nil 'nil 'nil 'nil)
        (list 'nil 'nil 'nil 'nil 'nil 'nil 15   'nil 8    1    5    3    'nil 4    11   7)
        (list 6    'nil 1    'nil 'nil 12   8    'nil 9    'nil 'nil 2    'nil 'nil 3    'nil)
        (list 14   'nil 4    13   6    'nil 'nil 3    'nil 12   7    10   8    'nil 2    'nil)
        (list 3    8    'nil 'nil 4    7    2    'nil 6    'nil 'nil 'nil 'nil 12   16   5)
        (list 13   'nil 'nil 16   'nil 8    14   10   3    4    15   'nil 12   5    1    11)
        (list 'nil 'nil 'nil 6    2    'nil 'nil 1    10   'nil 11   'nil 15   3    'nil 9)
        (list 7    'nil 'nil 12   'nil 4    'nil 15   5    'nil 9    14   'nil 'nil 'nil 'nil)
        (list 10   'nil 'nil 8    'nil 'nil 11   'nil 'nil 'nil 1    12   4    'nil 13   16)
        (list 'nil 'nil 'nil 'nil 'nil 'nil 7    'nil 15   2    'nil 'nil 'nil 'nil 12   3)
        (list 'nil 'nil 7    'nil 'nil 10   6    'nil 1    8    'nil 13   11   'nil 9    14)
        (list 8    6    5    'nil 'nil 3    'nil 'nil 14   'nil 'nil 9    'nil 'nil 'nil 'nil)
        (list 'nil 16   'nil 2    'nil 'nil 'nil 14   'nil 10   'nil 'nil 'nil 'nil 'nil 'nil)))

(define sudoku-problem-harder-fourbyfour
  (list (list 8 'nil 'nil 3 'nil 'nil 7 'nil 'nil 'nil 15 'nil 14 'nil 'nil 11)
        (list 'nil 11 'nil 9 'nil 3 'nil 'nil 'nil 16 'nil 13 1 'nil 15 7)
        (list 'nil 5 12 7 15 'nil 'nil 'nil 9 10 11 'nil 'nil 'nil 'nil 'nil)
        (list 'nil 'nil 'nil 4 'nil 11 'nil 9 'nil 'nil 5 1 'nil 'nil 'nil 10)
        (list 'nil 4 1 5 'nil 'nil 15 10 11 'nil 'nil 'nil 'nil 3 'nil 'nil)
        (list 'nil 'nil 6 'nil 'nil 'nil 'nil 'nil 2 12 'nil 'nil 7 1 10 'nil)
        (list 2 'nil 'nil 'nil 7 'nil 'nil 'nil 'nil 8 'nil 10 'nil 16 'nil 13)
        (list 'nil 'nil 'nil 13 6 'nil 'nil 'nil 'nil 1 'nil 3 2 15 'nil 'nil)
        (list 'nil 'nil 3 8 1 'nil 2 'nil 'nil 'nil 'nil 14 5 'nil 'nil 'nil)
        (list 4 'nil 10 'nil 12 'nil 5 'nil 'nil 'nil 'nil 7 'nil 'nil 'nil 8)
        (list 'nil 12 16 15 'nil 'nil 14 8 'nil 'nil 'nil 'nil 'nil 7 'nil 'nil)
        (list 'nil 'nil 14 'nil 'nil 'nil 'nil 13 1 15 'nil 'nil 16 12 3 'nil)
        (list 9 'nil 'nil 'nil 2 7 'nil 'nil 13 'nil 8 'nil 10 'nil 'nil 'nil)
        (list 'nil 'nil 'nil 'nil 'nil 14 10 5 'nil 'nil 'nil 16 9 8 2 'nil)
        (list 1 2 'nil 6 11 'nil 8 'nil 'nil 'nil 10 'nil 3 'nil 7 'nil)
        (list 15 'nil 'nil 10 'nil 16 'nil 'nil 'nil 3 'nil 'nil 11 'nil 'nil 12)))

(define sudoku-problem-fivebyfive
  (list (list 1    'nil 4    'nil 25   'nil 19   'nil 'nil 10   21   8    'nil 14   'nil 6    12   9    'nil 'nil 'nil 'nil 'nil 'nil 5)
        (list 5    'nil 19   23   24   'nil 22   12   'nil 'nil 16   6    'nil 20   'nil 18   'nil 25   14   13   10   11   'nil 1    15)
        (list 'nil 'nil 'nil 'nil 'nil 'nil 21   5    'nil 20   11   10   'nil 1    'nil 4    8    24   23   15   18   'nil 16   22   19)
        (list 'nil 7    21   8    18   'nil 'nil 'nil 11   'nil 5    'nil 'nil 24   'nil 'nil 'nil 17   22   1    9    6    25   'nil 'nil)
        (list 'nil 13   15   'nil 22   14   'nil 18   'nil 16   'nil 'nil 'nil 4    'nil 'nil 'nil 19   'nil 'nil 'nil 24   20   21   17)
        (list 12   'nil 11   'nil 6    'nil 'nil 'nil 'nil 15   'nil 'nil 'nil 'nil 21   25   19   'nil 4    'nil 22   14   'nil 20   'nil)
        (list 8    'nil 'nil 21   'nil 16   'nil 'nil 'nil 2    'nil 3    'nil 'nil 'nil 'nil 17   23   18   22   'nil 'nil 'nil 24   6)
        (list 4    'nil 14   18   7    9    'nil 22   21   19   'nil 'nil 'nil 2    'nil 5    'nil 'nil 'nil 6    16   15   'nil 11   12)
        (list 22   'nil 24   'nil 23   'nil 'nil 11   'nil 7    'nil 'nil 4    'nil 14   'nil 2    12   'nil 8    5    19   'nil 25   9)
        (list 20   'nil 'nil 'nil 5    'nil 'nil 'nil 'nil 17   9    'nil 12   18   'nil 1    'nil 'nil 7    24   'nil 'nil 'nil 13   4)
        (list 13   'nil 'nil 5    'nil 2    23   14   4    18   22   'nil 17   'nil 'nil 20   'nil 1    9    21   12   'nil 'nil 8    11)
        (list 14   23   'nil 24   'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil 20   25   'nil 3    4    13   'nil 11   21   9    5    18   22)
        (list 7    'nil 'nil 11   17   20   24   'nil 'nil 'nil 3    4    1    12   'nil 'nil 6    14   'nil 5    25   13   'nil 'nil 'nil)
        (list 'nil 'nil 16   9    'nil 17   11   7    10   25   'nil 'nil 'nil 13   6    'nil 'nil 18   'nil 'nil 19   4    'nil 'nil 20)
        (list 6    15   'nil 19   4    13   'nil 'nil 5    'nil 18   11   'nil 'nil 9    8    22   16   25   10   7    'nil 'nil 'nil 'nil)
        (list 'nil 'nil 'nil 2    'nil 'nil 10   19   3    'nil 1    'nil 22   9    4    11   15   'nil 20   'nil 'nil 8    23   'nil 25)
        (list 'nil 24   8    13   1    'nil 'nil 4    20   'nil 17   14   'nil 'nil 18   'nil 16   22   5    'nil 11   'nil 10   'nil 'nil)
        (list 23   10   'nil 'nil 'nil 'nil 'nil 'nil 18   'nil 6    'nil 16   'nil 'nil 17   1    'nil 13   'nil 'nil 3    19   12   'nil)
        (list 25   5    'nil 14   11   'nil 17   'nil 8    24   13   'nil 19   23   15   9    'nil 'nil 12   'nil 20   'nil 22   'nil 7)
        (list 'nil 'nil 17   4    'nil 22   15   'nil 23   11   12   25   'nil 'nil 'nil 'nil 18   8    'nil 7    'nil 'nil 14   'nil 13)
        (list 19   6    23   22   8    'nil 'nil 1    25   4    14   2    'nil 3    7    13   10   11   16   'nil 'nil 'nil 'nil 'nil 'nil)
        (list 'nil 4    'nil 17   'nil 3    'nil 24   'nil 8    20   23   11   10   25   22   'nil 'nil 'nil 12   13   2    18   6    'nil)
        (list 'nil 'nil 7    16   'nil 'nil 6    17   2    21   'nil 18   'nil 'nil 'nil 19   'nil 'nil 8    'nil 'nil 'nil 'nil 4    'nil)
        (list 18   9    25   1    2    11   'nil 'nil 13   22   4    'nil 21   'nil 5    'nil 23   7    'nil 'nil 15   'nil 3    'nil 8)
        (list 'nil 21   10   'nil 'nil 12   'nil 20   16   'nil 19   'nil 'nil 'nil 'nil 15   14   4    2    18   23   25   11   7    'nil)))


(define sudoku-problem-twobytwo
  (list (list 'nil 'nil 3    4 )
        (list 'nil 'nil 'nil 'nil)
        (list 'nil 'nil 'nil 'nil)
        (list 4    2    'nil 'nil)
        ))

;(display "\nEasiest sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-easiest)(newline)

;(display "\nEasy sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-easy)(newline)

;(display "\nMedium sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-medium)(newline)

;(display "\nHard sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-hard)(newline)

;(display "\nExpert sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-expert)(newline)

;(display "\nEvil sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-evil)(newline)

(display "\nWorlds Hardest sudoku problem solution: ")(newline)
(sudoku-solver sudoku-problem-world-hardest)(newline)

;(display "\nsudoku-problem-twobytwo sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-twobytwo)(newline)

;(display "\nsudoku-problem-harder-fourbyfour sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-harder-fourbyfour)(newline)

;(display "\nsudoku-problem-fourbyfour sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-fourbyfour)(newline)

(define nil-count 0)
(define (count-nil problem)
  (map
   (lambda(x)
     (map
      (lambda(y)
        (cond
          ((eq? y 'nil)
           (set! nil-count (+ nil-count 1))
           )
          )
        )
      x))
   problem)
  nil-count
  )

;(count-nil sudoku-problem-fivebyfive)

;(display "\nsudoku-problem-fivebyfive sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-fivebyfive)(newline)
