#lang racket

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
   
;; Takes a list of sets representing a single row of possible moves on the matrix and returns a list of squares
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
(define (transform matrix)
  (list-possible matrix))
    
(define poss-matrix (transform unsolved))

(define test-row (sixth poss-matrix))
        
;; The structure of each cell we will work with when trying to solve the puzzle
(struct square (grid row col value) #:transparent)  
  
;; finds all instances of x in the list and replaces them with y
(define (replace lst x y)
 (cond
   [(empty? lst) lst]
   [(list? (first lst)) (cons (replace (first lst) x y) (replace (rest lst) x y))]
   [(equal? (first lst) x) (cons y (replace (rest lst) x y))]
   [else (cons (first lst) (replace (rest lst) x y))]))

;; extracts a sub-list from a matrix beginning at the cell with co-ordinates (row,column)
;; and comprised of the "sublist-size" adjacent cells to the right of the first cell.  Sub-list
;; elements cannot span more than one row, e.g. (end of row - first cell >= sublist-size) 
(define (extract matrix row column sublist-size)
 (take (drop (car (take (drop matrix row)1))column)sublist-size))

;; calls the replace method with the search key and replace values listed on the
;; sub-list beginning at cell (row,column) and ending "sublist-size" cells to the right
(define (manipulate matrix row column sublist-size)
 (let* ((initial-value (extract matrix row column sublist-size))
       (possible-value (make-set initial-value)))
   (replace (extract matrix row column sublist-size) (car initial-value) possible-value)))


;; where row and column parameters indicate the first cell to transform and
;; sublist-size indicates the number of adjacent cells you want to transform
;; the matrix rows and columns are numbered 0 to 8
(define (trans matrix row column sublist-size)
 (let ([inner (take matrix row)]  ; select the first "row" number of rows in the matrix (rows 0 to "row" - 1)
       [remainder (drop matrix row)])  ; remove selected number of rows from the matrix to produce a new matrix beginning at row number "row"
   (append inner 
         (append 
          (list
          (flatten
           (list
            (take (first (take remainder 1)) column)  ; select the columns before the sublist (to remain unchanged) on the row to be manipulated
            (manipulate matrix row column sublist-size) ; manipulate the values of the selected sublist
            (drop (first (take remainder 1)) (+ column sublist-size))))) ; select the remaining columns on the manipulated row after the sublist (to remain unchanged)         
          (drop matrix (+ row 1))))))

;; returns a row in the matrix
(define (get-row matrix row)
  (extract matrix row 0 9))

;; returns a column in the matrix
(define (get-column matrix col)
  (append (extract matrix 0 col 1)
          (extract matrix 1 col 1)
          (extract matrix 2 col 1)
          (extract matrix 3 col 1)
          (extract matrix 4 col 1)
          (extract matrix 5 col 1)
          (extract matrix 6 col 1)
          (extract matrix 7 col 1)
          (extract matrix 8 col 1)))

;; returns true if the cell co-ordinates are for a singleton set
(define (is-singleton matrix row column)
  (if (= (set-count (car (take (drop matrix 0) 1))) 1)
      #t
      #f))

;; returns true if set is a singleton
(define (singleton cell)
  (if (= (set-count cell) 1)
      #t
      #f))


;; returns singleton elements in a list
(define (singleton-list lst)
  (filter (lambda (x) (eq? (set-count x) 1)) lst))

;; returns the value of a singleton set
(define (singleton-value cell)
  (set-first cell))

;; Helper function to find out the index number of an element in a list 
(define (list-index list element)
  (let loop ((any-list list)
             (index 0))
    (cond ((empty? any-list) #f)
          ((equal? (first any-list) element) index)
          (else (loop (rest any-list) (add1 index))))))

;; Find a location containing a singleton set (a set containing just one number).
;; For every other set in the same row, the same column, or the same 3x3 box, remove that number (if present).
(define (singleton-search matrix)
  (for/list ([i (in-naturals 1)]
             [matrix])
    (let* ((row-options (get-row matrix i))
          (singletons-present (singleton-list row-options)))
      
;; 
(define 
      (for/list ([j (in-naturals 1)]
                 [singletons-present])
        (let* ((val (singleton-valuecol-index (list-index row-options
                  

(provide list-index
         unsolved
         possible
         which-grid
         square
         make-set
         list-possible
         new-squares
         create-new-rows
         transform)
         
    
       
      
