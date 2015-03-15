#lang racket

;; Helper function to find out the index number of an element in a list - decided not to use yet but may use later?
(define (list-index list element)
  (let loop ((any-list list)
             (index 0))
    (cond ((empty? any-list) #f)
          ((equal? (first any-list) element) index)
          (else (loop (rest any-list) (add1 index))))))

;; Example input list describing an initialised Sudoku board
(define unsolved
  '((0 2 5 0 0 1 0 0 0)
    (1 0 4 2 5 0 0 0 0)
    (0 0 6 0 0 4 2 1 0)
    (0 5 0 0 0 0 3 2 0)
    (6 0 0 0 2 0 0 0 9)
    (0 8 7 0 0 0 0 6 0)
    (0 9 1 5 0 0 6 0 0)
    (0 0 0 0 7 8 1 0 3)
    (0 0 0 6 0 0 5 9 0)))

;; Set of all possible values
(define possible (seteq 1 2 3 4 5 6 7 8 9))

;; Sets grid ID for the 9 3X3 grids on the matrix
(define (which-grid r c)
  (cond ((and (and (>= r 0) (< r 3)) 
             (and (>= c 0) (< c 3))) 1)
        ((and (and (>= r 0) (< r 3)) 
             (and (>= c 3) (< c 6))) 2)
        ((and (and (>= r 0) (< r 3)) 
             (and (>= c 6) (< c 9))) 3)
        ((and (and (>= r 3) (< r 6)) 
             (and (>= c 0) (< c 3))) 4)
        ((and (and (>= r 3) (< r 6)) 
             (and (>= c 3) (< c 6))) 5)
        ((and (and (>= r 3) (< r 6)) 
             (and (>= c 6) (< c 9))) 6)
        ((and (and (>= r 6) (< r 9)) 
             (and (>= c 0) (< c 3))) 7)
        ((and (and (>= r 6) (< r 9)) 
             (and (>= c 3) (< c 6))) 8)
        ((and (and (>= r 6) (< r 9)) 
             (and (>= c 6) (< c 9))) 9)
        (else 0)))

;; The structure of each cell we will work with when trying to solve the puzzle
(struct square (grid row col poss-list) #:transparent)

;; Helper function to choose what set to substitue for the intial value in the input list
(define (make-set x)
  (if (eq? x 0)
      possible
      (seteq x)))

;; Reads the input list and returns a list of sets depicting all possible moves in each cell
(define (list-possible unsolved)
  (for/list ([x unsolved])
    (if (list? x)
        (map make-set x)
        (printf "problem: ~a is not a list" x))))
   
;; Takes a list of sets representing a single row on the matrix and returns a list of squares
(define (new-squares poss-moves rowNum)
  (let ((square-list empty))
  (for ([i 9])
    (let* ((move-set (list-ref poss-moves i))
          (column i)
          (grid (which-grid rowNum column)))
      (drop (cons square-list (square grid rowNum column move-set)) 1)))))

;; Splits up the possibility set matrix into rows to try and generate the new matrix of squares
(define (create-new-rows poss-sets)
  (let ((new-matrix empty))
  (for ([i 9])
      (let ((poss-row (list-ref poss-sets i)))
        (drop (cons new-matrix (new-squares poss-row i)) 1)))))

;; takes a Sudoku puzzle in the above list of lists format, 
;; and replaces each integer with a set of integers, thus returning a list of sets of integers.  
(define (transform unsolved)
  (let ((poss-sets (list-possible unsolved)))
    (create-new-rows poss-sets)))
    
    
       
      
