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
      (cond
        ((or (= x 0) (= x 1) (= x 2)) 0)
        ((or (= x 3) (= x 4) (= x 5))  box-size)
        (else (* box-size 2))        
        )
      )

    (define (get-starting-column-position x)
      (cond
        ((or (= x 0) (= x 3) (= x 6)) 0)
        ((or (= x 1) (= x 4) (= x 7))  box-size)
        (else (* box-size 2))        
        )
      )
    
     (define box-starting-row-position (get-starting-row-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))
     (define box-starting-column-position (get-starting-column-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))


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
     
      (list temporary-column-values (cadr column))
      )
    
    (define (get-updated-column-value column-list row-count)      
      (define count 0)
      
      (cond
        ((eq? row-count row-position)
         (map
       (lambda(x)
         (cond ((eq? count column-position)
               
               (set! count (+ count 1))
               (list newValue (cadr x))
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
      (cond
        ((or (= x 0) (= x 1) (= x 2)) 0)
        ((or (= x 3) (= x 4) (= x 5))  box-size)
        (else (* box-size 2))        
        )
      )

    (define (get-starting-column-position x)
      (cond
        ((or (= x 0) (= x 3) (= x 6)) 0)
        ((or (= x 1) (= x 4) (= x 7))  box-size)
        (else (* box-size 2))        
        )
      )
    
     (define box-starting-row-position (get-starting-row-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))
     (define box-starting-column-position (get-starting-column-position (cadr (list-ref (list-ref all-possiblestates row-position) column-position))))

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

    (define (update-sudoku-solution-with-single-possible-move)
      ;(display "\nInside update solution with single possible move") (newline)
      
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
    
    ;(update-sudoku-solution-with-single-possible-move)
    )

  
  ;Solve sudoku problem
  (define (SolveSudokuProblem)    
     (DepthFirstSearch Path nextMove checkFinalState MaxDepth update-list-position InitialIndex)     
    )
  
   (define sudoku-solution sudoku-problem)   
   

  

  (define maxMoves (length sudoku-problem))
  (define box-size (sqrt maxMoves))
  (define Possiblemoves (enumerate-interval 1 maxMoves))
  
  (display "\nBox size: ")
  (display box-size) (newline)
  
  (define (box-position row-position column-position)
    (define row-value (quotient row-position box-size))
    (define column-value (quotient column-position box-size))
    (cond
      ((= row-value 0)
       (cond
         ((= column-value 0) 0)
         ((= column-value 1) 1)
         (else 2)
         )
       )
      ((= row-value 1)
       (cond
         ((= column-value 0) 3)
         ((= column-value 1) 4)
         (else 5)
         )
       )
      (else
       (cond
         ((= column-value 0) 6)
         ((= column-value 1) 7)
         (else 8)
         )
       )
      )
    )

  (define all-possiblestates
    (map
     (lambda(x)
       (map
        (lambda(y) (list Possiblemoves (box-position x y)))
        (enumerate-interval 0 (- (length (car sudoku-solution)) 1)))
       )
     (enumerate-interval 0 (- (length sudoku-solution) 1)))
    )
  
  
   
   (define InitialIndex 0)
  
  (define InitialState (list (list 0 0) InitialIndex))
   (define Path (list InitialState))
   (define MaxDepth (* maxMoves maxMoves))

  (ConstraintPropagation)

  (display "\nall possible states after constraint propagation: ")
  (display all-possiblestates) (newline)

  (define final-solution null)
  (cond
    ((not (find-next-empty-position 0 0))
     (set! final-solution (list-tail sudoku-solution 1))
     (display "Solution found only by constraint propagation")(newline)
     (display "\nFinal Solution: ")(newline)
     (display final-solution)
     
    )
    (else
     (set! final-solution (list-tail (car (SolveSudokuProblem)) 1))
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

(define sudoku-problem-fourbyfour
  (list (list 'nil 9    'nil 14   'nil 'nil 13   'nil 2    7    16   'nil 'nil 15    6    'nil)
        (list 12   'nil 16   'nil 'nil 2    'nil 'nil 'nil 4    'nil 3    'nil 13    'nil 'nil)
        (list 3    'nil 5    8    'nil 'nil 'nil 'nil 'nil 12   'nil 6    1    4     'nil 'nil)
        (list 'nil 15   'nil 13   'nil 9    'nil 11   'nil 'nil 'nil 5    'nil 'nil  'nil 'nil)
        (list 4    8    7    'nil 'nil 6    'nil 'nil 14   'nil 'nil 'nil 'nil 3     'nil 'nil)
        (list 9    'nil 'nil 3    'nil 'nil 10   'nil 12   6    8    'nil 'nil 'nil  11   16)
        (list 5    14   'nil 10 'nil 'nil 'nil 3 'nil 1 'nil 9 'nil 7 4 'nil)
        (list 'nil 'nil 'nil 16 'nil 12 4 'nil 'nil 'nil 'nil 10 'nil 'nil 'nil 8)
        (list 14 'nil 'nil 'nil 5 'nil 'nil 'nil 'nil 2 15 'nil 7 'nil 'nil 'nil)
        (list 'nil 11 13 'nil 6 'nil 3 'nil 16 'nil 'nil 'nil 15 'nil 8 14)
        (list 1 6 'nil 'nil 'nil 10 9 14 'nil 5 'nil 'nil 12 'nil 'nil 2)
        (list 'nil 'nil 12 'nil 'nil 'nil 'nil 13 'nil 'nil 10 'nil 'nil 1 9 5)
        (list 'nil 'nil 'nil 'nil 13 'nil 'nil 'nil 1 'nil 12 'nil 3 'nil 5 'nil)
        (list 'nil 'nil 3 15 14 'nil 2 'nil 'nil 'nil 'nil 'nil 16 6 'nil 13)
        (list 'nil 'nil 'nil 1 'nil 3 'nil 16 'nil 'nil 'nil 13 'nil 'nil 12 9)
        (list 'nil 5 9 'nil 'nil 8 1 6 'nil 16 'nil 'nil 10 'nil 7 'nil)))

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

;(display "\nWorlds Hardest sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-world-hardest)(newline)

;(display "\nsudoku-problem-fourbyfour sudoku problem solution: ")(newline)
;(sudoku-solver sudoku-problem-fourbyfour)(newline)

(display "\nsudoku-problem-twobytwo sudoku problem solution: ")(newline)
(sudoku-solver sudoku-problem-twobytwo)(newline)
