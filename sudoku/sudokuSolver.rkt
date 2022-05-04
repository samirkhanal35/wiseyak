#lang racket
(define (DepthFirstSearch Path nextMove checkFinalState MaxDepth)

  (display "\n Inside DFS") (newline)
  (display "passed Path")
  (display Path) (newline)
  
  (define (LastStateIndex)
    (define lastIndex (- (length Path) 1))
    (define lastStateIndex (list-ref Path lastIndex))
     (list-ref lastStateIndex 1)
    )
  
  (define (firstIndex)
     (list-ref (list-ref Path 0) 1)
     )
  
  (define (RemoveLastState)  
   (define lastIndex (- (length Path) 1))
   (define lastStateIndex (list-ref Path lastIndex))    
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
  (display "\n Inside sudoku-solver") (newline)
  (display "Passed sudoku problem:")
  (display sudoku-problem) (newline)   

  (define (find-next-empty-position i j)
    (display "\nInside find next empty position")(newline)
    (display "value of i: ")
      (display i) (newline)
      (display "value of j: ")
      (display j) (newline)
      (display "sudoku solution list: ")
      (display sudoku-solution) (newline)
    
    (define next-expty-position null)
    (define (find-i-and-jth-value i j)
      (display "\nInside find i and j") (newline)
      
      
      (if (eq? (list-ref (list-ref sudoku-solution i) j) 'nil)
          (list i j)
          (cond
            ((and (< i 9) (< j 9)) (find-i-and-jth-value (+ i 1) (+ j 1)))
            ((and (> i 9) (< j 9)) (find-i-and-jth-value i (+ j 1)))
            ((and (< i 9) (> j 9)) (find-i-and-jth-value (+ i 1) j))
            (else '#f)
            ))
      )
    (find-i-and-jth-value i j)
    )
 
   (define (CheckLegality StateIndex)
     (display "\nInside CheckLegality")(newline)
     (display "passed stateValue:")
     (display StateIndex) (newline)
     
     (define row-position (car (car StateIndex)))
     (define column-position (cadr (car StateIndex)))    
     (define newValue (cadr StateIndex))

     (define (box-starting-index position-value)
       (cond
         ((or (and (= (floor (/ position-value 3)) 0) (> (remainder position-value 3) 0))
              (and (= (floor (/ position-value 3)) 1) (= (remainder position-value 3) 0)))
          0
          )
         ((or (and (= (floor (/ position-value 3)) 1) (> (remainder position-value 3) 0))
              (and (= (floor (/ position-value 3)) 2) (= (remainder position-value 3) 0)))
          3
          )
         ((or (and (= (floor (/ position-value 3)) 2) (> (remainder position-value 3) 0))
              (and (= (floor (/ position-value 3)) 3) (= (remainder position-value 3) 0)))
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

   (define (checkFinalState state)
     (display "\nInside Check Final State")
     (display "\nPassed state: ")
     (display state) (newline)
     (if (find-next-empty-position (car state) (cadr state))
         '#f
         '#t
         )
     )

   (define (nextMove CurrentState prevIndex)
     (display "\nInside NextMove")
     (display "\nSudoku solution: ")
     (display sudoku-solution) (newline)
     (display "passed CurrentState: ")
     (display CurrentState) (newline)
    
     (define newStateIndex 0)
     (define newStateIndexFlag '#f)
     (define StartingIndex (+ prevIndex 1))
     
     (define (nextMoveState)
       (cond ((< StartingIndex maxMoves)              
              (define Move (list-ref Possiblemoves StartingIndex))
              (define newposition (find-next-empty-position (car CurrentState) (cadr CurrentState)))
              (display "\nNew empty position")
              (display newposition) (newline)
              
              (set! newStateIndex (list newposition StartingIndex))

              (display "\nNew position with value")
              (display newStateIndex) (newline)
        (cond  ((CheckLegality newStateIndex)                
                (set! newStateIndexFlag '#t))
               (else                
                (set! StartingIndex (+ StartingIndex 1))               
                (nextMoveState))
               )
              )
         )
       (cond (newStateIndexFlag
              (set! sudoku-solution (update-list-position sudoku-solution newStateIndex))
              newStateIndex)
             (else '#f)
             )            
       )
     (nextMoveState)
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
        ((eq? (car list1) element) (car list1))
        (else (check (cdr list1)))
        ))
    (check list1)
    )

  (define (update-list-position sudoku-solution newStateIndex)
    (display "\nInside update list position") (newline)
    (display "passed sudoku-solution: ")
    (display sudoku-solution) (newline)
    (display "passed newstateIndex: ")
    (display newStateIndex) (newline)
    
    (define row-position (car (car newStateIndex)))
    (define column-position (cadr (car newStateIndex)))    
    (define newValue (cadr newStateIndex))

    (display "row position: ")
    (display row-position) (newline)
    (display "column position: ")
    (display column-position) (newline)

    (define row (length sudoku-solution))
    (define column (length (car sudoku-solution)))

    (display "row length: ")
    (display row) (newline)
    (display "column length: ")
    (display column) (newline)
    

    (define (get-updated-column-value column-list column-count)
      (display "\nInside get updated column value") (newline)
      
      (define column-list-values null)
      (define count 1)
      
      (define (get-list-values count)
        (cond
          ((= count column-count)
           (cond ((eq? column-count column-position)
               (set! column-list-values (append-to-list column-list-values newValue))
               (get-list-values (+ count 1))
                              
               )
                 (else
                  (set! column-list-values (append-to-list column-list-values (list-ref column-list)))
                  (get-list-values (+ count 1))
                  )
                 )
           )
          (else
           column-list-values)
          )
         )
      (get-list-values count)
        )
      

    (define row-count 1)

    (map (lambda(x)
           (cond ((eq? row-count row-position)
               (get-updated-column-value x row-count)               
               )
                 (else (set! row-count (+ row-count 1))
                x))
           ) sudoku-solution)    
    )

  
  
  (define (SolveSudokuProblem)
    
    (define solution (DepthFirstSearch Path nextMove checkFinalState MaxDepth))
    (set! Path  (list-ref solution 0))
    (define solutionExists (list-ref solution 1))
    (cond ((> (length Path) 1)
           (display "\nSolution:")
           (display Path)
           (display "\n")
          (display "\nEnter 'y' for next solution: \n")
          (define userInput (read))
          (cond ((equal? userInput 'y) (SolveSudokuProblem))))
          (else
           (display "\nSorry!! Solution doesn't exist.")))
       
    
    )
  

   (define sudoku-solution sudoku-problem)
   
   (define Possiblemoves (list 1 2 3 4 5 6 7 8 9))
   (define maxMoves 9)
   ;(define InitialIndex 0)
  (define InitialState (nextMove (list 0 0) 0 ))
   

  ;(define Path (list (list InitialState InitialIndex)))
  (define Path (list InitialState))
  (define MaxDepth 2)

  (display "InitialState: ")
  (display InitialState) (newline)
  (display "Path:")
  (display Path) (newline)
  (display "sudoku solution: ")
  (display sudoku-solution) (newline)
  
  (SolveSudokuProblem)
 )


(define sudoku-problem
  (list (list 4 'nil 'nil 'nil 'nil 'nil 'nil 2 'nil)
        (list 'nil 'nil 'nil 5 1 'nil 6  'nil  7 )
        (list 'nil 'nil 5 'nil 2 7 'nil 'nil 'nil)
        (list 3 'nil 6 'nil 'nil 'nil 9 7 'nil)
        (list 5 'nil 'nil 'nil 'nil 3 4 'nil 2)
        (list 'nil 'nil 7 1 'nil 'nil 'nil 'nil 'nil)
        (list 'nil 9 4 6 'nil 'nil 'nil 'nil 'nil )
        (list 'nil 2 8 4 3 1 7 5 'nil)
        (list 'nil 5 3 'nil 7 9 8 6 4)))


(sudoku-solver sudoku-problem)