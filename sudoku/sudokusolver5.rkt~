#lang racket
(define (DepthFirstSearch Path nextMove checkFinalState MaxDepth update-list-position initialIndex)

  (display "\n Inside DFS") (newline)
  (display "passed Path")
  (display Path) (newline)
  
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
    (define updated-sudoku (update-list-position lastStateIndex))
   (set! Path (reverse (remove lastStateIndex (reverse Path))))    
   (list-ref lastStateIndex 1)
   )
  
  (define (getLast-State)  
   (list-ref (list-ref Path (- (length Path) 1)) 0)
   )
  
  (define prevIndex (LastStateIndex))
  (define Branch-pruned '#f)  
  (define (DFS-Inner-Loop)    
  (define LastState (getLast-State))    
  (define NextStateIndex (nextMove LastState prevIndex))
    (display "\nInside DFS, after nextMove") (newline)
    (display "generated nextstateindex: ")
    (display NextStateIndex) (newline)
  (define NextState  0)    
    
   (cond (NextStateIndex        
        (set! Path (append Path (list NextStateIndex)))
        (display "updated Path: ")
        (display Path) (newline)
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
    (display "\nInside update list position") (newline)
    
    (display "passed newstateIndex: ")
    (display newStateIndex) (newline)
    
    (define row-position (car (car newStateIndex)))
    (define column-position (cadr (car newStateIndex)))    
    (define newValue (cadr newStateIndex))

    (display "row position: ")
    (display row-position) (newline)
    (display "column position: ")
    (display column-position) (newline)
    (display "to update value: ")
    (display newValue) (newline)

    (define row (length sudoku-solution))
    (define column (length (car sudoku-solution)))

    (display "row length: ")
    (display row) (newline)
    (display "column length: ")
    (display column) (newline)
    

    (define (get-updated-column-value column-list)
      (display "\nInside get updated column value") (newline)
      (display "passed column list: ")
      (display column-list) (newline)     
      
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

     (map (lambda(x)
           (cond ((eq? row-count row-position)
                  (set! row-count (+ row-count 1))
                (get-updated-column-value x)               
               )
                 (else
                  (set! row-count (+ row-count 1))
                  x
                  ))
           ) sudoku-solution)    
    
    )


  
  (define (find-next-empty-position i j)
    (display "\nInside find next empty position")(newline)
    (display "value of i: ")
      (display i) (newline)
      (display "value of j: ")
      (display j) (newline)    
    
    
    (define (next-i-j-values)
      (cond ((and (< i 9) (< j 8))
             (set! j (+ j 1)))
            ((and (< i 8) (= j 8))
             (set! i (+ i 1))
             (set! j 0))
            )
      )
    (next-i-j-values)
    (define (find-i-and-jth-value)
      (display "\nInside find i and j") (newline)
      (display "i: ")
      (display i) (newline)
      (display "j: ")
      (display j) (newline)
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
    (display "\nInside append to list")
    (display "passed list")
    (display list1) (newline)
    (display "passed append to list: ")
    (display to-append-list) (newline)
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
     (display "\nInside Check Final State")
     
     (cond ((find-next-empty-position (car state) (cadr state))
            (display "\nfind-next empty position returned true in checkFinal state")(newline)         
         '#f)
           (else
         '#t
         )))
    
  (define (CheckLegality StateIndex)
     (display "\nInside CheckLegality")(newline)
    
     
     (define row-position (car (car StateIndex)))
     (define column-position (cadr (car StateIndex)))    
     (define newValue (cadr StateIndex))

     (display "row position: ")
     (display row-position) (newline)
     (display "column position: ")
     (display column-position) (newline)

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


  (define (nextMove CurrentPosition prevIndex)
     (display "\nInside NextMove")(newline)
     
     (display "passed CurrentPosition ")
     (display CurrentPosition) (newline)
    
     (define newStateIndex 0)
     (define newStateIndexFlag '#f)
     (define StartingIndex (+ prevIndex 1))

     (define row-position (car CurrentPosition))
     (define column-position (cadr CurrentPosition))
     
     (define (nextMoveState)
       (cond ((< StartingIndex (+ maxMoves 1))            
                            
              (define newposition (find-next-empty-position row-position column-position))
              (display "\nNew empty position")
              (display newposition) (newline)
              
              (set! newStateIndex (list newposition StartingIndex))

              (display "\nNew position with value")
              (display newStateIndex) (newline)
        (cond  ((CheckLegality newStateIndex)
                (display "\nPassed Legality Test") (newline)
                (set! newStateIndexFlag '#t))
               (else                
                (set! StartingIndex (+ StartingIndex 1))               
                (nextMoveState))
               )
              )
         )
       (cond (newStateIndexFlag
              (set! sudoku-solution (update-list-position newStateIndex))
              newStateIndex)
             (else '#f)
             )            
       )
     (nextMoveState)
      )

  
  (define (SolveSudokuProblem)    
     (DepthFirstSearch Path nextMove checkFinalState MaxDepth update-list-position InitialIndex)     
    )
  
   (define sudoku-solution sudoku-problem)
   
   (define Possiblemoves (list 1 2 3 4 5 6 7 8 9))
   (define maxMoves 9)
   (define InitialIndex 0)
   (define InitialState (nextMove (list 0 0) InitialIndex))
   

   ;(define Path (list (list InitialState InitialIndex)))
   (define Path (list InitialState))
   (define MaxDepth 10)

   (display "InitialState: ")
   (display InitialState) (newline)
   (display "Path:")
   (display Path) (newline)
   (display "sudoku solution: ")
   (display sudoku-solution) (newline)

  ;(set! sudoku-solution (update-list-position InitialState))

 
   
  (SolveSudokuProblem)
 )


(define sudoku-problem
  (list (list 3 1 6 'nil 7 8 4 9 2)
        (list 5 2 9 1 3 4 7 6 8)
        (list 4 8 7 6 'nil 9 5 3 1)
        (list 2 6 3 4 1 5 9 8 7)
        (list 9 7 4 8 6 3 1 2 5)
        (list 8 5 1 7 9 2 6 4 'nil)
        (list 1 3 8 9 4 7 2 5 6)
        (list 6 9 2 3 5 1 8 7 4)
        (list 7 4 5 2 8 6 3 'nil 9)))

(define result
  (list (list 3 1 6 5 7 8 4 9 2)
        (list 5 2 9 1 3 4 7 6 8)
        (list 4 8 7 6 2 9 5 3 1)
        (list 2 6 3 4 1 5 9 8 7)
        (list 9 7 4 8 6 3 1 2 5)
        (list 8 5 1 7 9 2 6 4 3)
        (list 1 3 8 9 4 7 2 5 6)
        (list 6 9 2 3 5 1 8 7 4)
        (list 7 4 5 2 8 6 3 1 9)))


(sudoku-solver sudoku-problem)