#lang racket

;; where row and column parameters indicate the first cell to transform and
;; sublist-size indicates the number of adjacent cells you want to transform
;; the matrix rows and columns are numbered 0 to 8
; (define (amend-set matrix row column sublist-size singleton)
;  (let ([inner (take matrix row)]  ; select the first "row" number of rows in the matrix (rows 0 to "row" - 1)
;       [remainder (drop matrix row)])  ; remove selected number of rows from the matrix to produce a new matrix beginning at row number "row"
;   (append inner 
;         (append 
;          (list
;          (flatten
;           (list
;            (take (first (take remainder 1)) column)  ; select the columns before the sublist (to remain unchanged) on the row to be manipulated
;            (remove-singleton-val matrix row column sublist-size '(seteq 1 2 3 4 5 6 7 8 9) singleton) ; manipulate the values of the selected sublist
;            (drop (first (take remainder 1)) (+ column sublist-size))))) ; select the remaining columns on the manipulated row after the sublist (to remain unchanged)         
;          (drop matrix (+ row 1))))))

;; calls the replace method with the search key and replace values listed on the
;; sub-list beginning at cell (row,column) and ending "sublist-size" cells to the right
; (define (remove-singleton-val matrix row column sublist-size init-val single)
;   (replace (extract matrix row column sublist-size) init-val (set-remove init-val single)))

;; The structure of each cell we will work with when trying to solve the puzzle
(struct square (grid row col value) #:transparent)  

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

;; takes a Sudoku puzzle in the above list of lists format, 
;; and replaces each integer with a set of integers, thus returning a list of sets of integers.  
(define (transform matrix)
  (list-possible matrix))

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

;; returns a list of all 9 cells in one of the grids on board.  Cells are listed in order from top left to bottom right.
(define (grid-cell-list matrix grid-num)
  (cond [(= grid-num 1) (append (extract matrix 0 0 3)
                               (extract matrix 1 0 3)
                               (extract matrix 2 0 3))]
        [(= grid-num 2) (append (extract matrix 0 3 3)
                               (extract matrix 1 3 3)
                               (extract matrix 2 3 3))]
        [(= grid-num 3) (append (extract matrix 0 6 3)
                               (extract matrix 1 6 3)
                               (extract matrix 2 6 3))]
        [(= grid-num 4) (append (extract matrix 3 0 3)
                               (extract matrix 4 0 3)
                               (extract matrix 5 0 3))]
        [(= grid-num 5) (append (extract matrix 3 3 3)
                               (extract matrix 4 3 3)
                               (extract matrix 5 3 3))]
        [(= grid-num 6) (append (extract matrix 3 6 3)
                               (extract matrix 4 6 3)
                               (extract matrix 5 6 3))]
        [(= grid-num 7) (append (extract matrix 6 0 3)
                               (extract matrix 7 0 3)
                               (extract matrix 8 0 3))]
        [(= grid-num 8) (append (extract matrix 6 3 3)
                               (extract matrix 7 3 3)
                               (extract matrix 8 3 3))]
        [(= grid-num 9) (append (extract matrix 6 6 3)
                               (extract matrix 7 6 3)
                               (extract matrix 8 6 3))]
        [else '()]))
         
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
  (let loop ((my-list list)
             (index 0))
    (cond ((empty? my-list) #f)
          ((equal? (first my-list) element) index)
          (else (loop (rest my-list) (add1 index))))))

;; calls the replace method with the search key and replaces the value in the list
(define (remove-from-list list init-val singleton)
  (replace (cons (car list) '()) init-val (set-remove init-val singleton)))

;; takes list of sets and reduces all sets by a particular value
(define (rec-reduce-choices list acc-list single)
  (cond
    [(empty? list) acc-list]
    [(set=? (car list) (seteq single)) (rec-reduce-choices (drop list 1) (append acc-list (cons (car list) '())) single)]
    [else (rec-reduce-choices (drop list 1) (append acc-list (remove-from-list list (car list) single)) single)]))

;; function to reduce row choices by removing a singleton from the row by calling rec-reduce-choices function
(define (reduce-row-choices matrix row single)
  (rec-reduce-choices (get-row matrix row) '() single))
  
;; function to reduce column choices by removing a singleton from the column by calling rec-reduce-choices function
(define (reduce-column-choices matrix col single)
  (rec-reduce-choices (get-column matrix col) '() single))

;; function to reduce grid choices by removing a singleton from the grid by calling rec-reduce-choices function
(define (reduce-grid-choices matrix grid-num single)
  (rec-reduce-choices (grid-cell-list matrix grid-num) '() single))

;; amends a row in the matrix by removing a specified singleton from every set in a specified row
(define (amend-row matrix row singleton)
 (let ([top-rows (take matrix row)]  ; select the first "row" number of rows in the matrix (rows 0 to "row" - 1)
       [remainder (drop matrix row)])  ; remove selected number of rows from the matrix to produce a new matrix beginning at row number "row"
   (append top-rows 
         (append 
          (list (reduce-row-choices matrix row singleton))      
          (drop matrix (+ row 1))))))

;; amends a column in the matrix by removing a specified singleton from every set in a specified column
(define (amend-column matrix column singleton)
  (cons
   (append
    (take (first matrix) column)  ; select the columns before the column (to remain unchanged) on the row to be manipulated
    (rec-reduce-choices (cons (list-ref (first matrix) column) '()) '() singleton) ; replace the value in the matrix column with the list value
    (drop (first matrix) (+ column 1))) ; select the remaining columns on the manipulated row after the inserted column value (to remain unchanged)
   (cons
     (append
      (take (second matrix) column)  
      (rec-reduce-choices (cons (list-ref (second matrix) column) '()) '() singleton) 
      (drop (second matrix) (+ column 1))) 
     (cons
     (append
      (take (third matrix) column)  
      (rec-reduce-choices (cons (list-ref (third matrix) column) '()) '() singleton) 
      (drop (third matrix) (+ column 1))) 
     (cons
     (append
      (take (fourth matrix) column)  
      (rec-reduce-choices (cons (list-ref (fourth matrix) column) '()) '() singleton) 
      (drop (fourth matrix) (+ column 1))) 
     (cons
     (append
      (take (fifth matrix) column)  
      (rec-reduce-choices (cons (list-ref (fifth matrix) column) '()) '() singleton) 
      (drop (fifth matrix) (+ column 1))) 
     (cons
     (append
      (take (sixth matrix) column)  
      (rec-reduce-choices (cons (list-ref (sixth matrix) column) '()) '() singleton) 
      (drop (sixth matrix) (+ column 1))) 
     (cons
     (append
      (take (seventh matrix) column)  
      (rec-reduce-choices (cons (list-ref (seventh matrix) column) '()) '() singleton) 
      (drop (seventh matrix) (+ column 1))) 
     (cons
     (append
      (take (eighth matrix) column)  
      (rec-reduce-choices (cons (list-ref (eighth matrix) column) '()) '() singleton) 
      (drop (eighth matrix) (+ column 1))) 
    (list
     (append
      (take (ninth matrix) column)  
      (rec-reduce-choices (cons (list-ref (ninth matrix) column) '()) '() singleton) 
      (drop (ninth matrix) (+ column 1)))))))))))))

;; returns the row number of the top row in the grid
(define (top-grid-row grid)
  (cond [(= grid 1) 0]
        [(= grid 2) 0]
        [(= grid 3) 0]
        [(= grid 4) 3]
        [(= grid 5) 3]
        [(= grid 6) 3]
        [(= grid 7) 6]
        [(= grid 8) 6]
        [(= grid 9) 6]))

;; returns the column number of the left-most column in the grid
(define (left-grid-col grid)
  (cond [(or (= grid 1) (or (= grid 4) (= grid 7))) 0]
        [(or (= grid 2) (or (= grid 5) (= grid 8))) 3]
        [(or (= grid 3) (or (= grid 6) (= grid 9))) 6]))

;; amends a grid in the matrix by removing a specified singleton from every set in a specified grid
(define (amend-grid matrix grid singleton)
  (let ([top-rows (take matrix (top-grid-row grid))]  ; select the first "row" number of rows in the matrix (rows 0 to "row" - 1)
        [remainder (drop matrix (+ 3 (top-grid-row grid)))])  ; remove selected number of rows from the matrix to produce a new matrix beginning at row number "row"
    (append top-rows 
            (append
             (cons 
              (append (take (get-row matrix (top-grid-row grid)) (left-grid-col grid))
                      (append (rec-reduce-choices (extract matrix (top-grid-row grid) (left-grid-col grid) 3) '() singleton)
                              (drop (get-row matrix (top-grid-row grid)) (+ 3 (left-grid-col grid)))))      
              (cons 
               (append (take (get-row matrix (+ 1 (top-grid-row grid))) (left-grid-col grid))
                       (append (rec-reduce-choices (extract matrix (+ 1 (top-grid-row grid)) (left-grid-col grid) 3) '() singleton)
                               (drop (get-row matrix (+ 1 (top-grid-row grid))) (+ 3 (left-grid-col grid)))))
               (list 
                (append (take (get-row matrix (+ 2 (top-grid-row grid))) (left-grid-col grid))
                        (append (rec-reduce-choices (extract matrix (+ 2 (top-grid-row grid)) (left-grid-col grid) 3) '() singleton)
                                (drop (get-row matrix (+ 2 (top-grid-row grid))) (+ 3 (left-grid-col grid))))))))
             remainder))))

;; remove singleton from all cells in the same row, column and grid as the given cell
(define (remove-singleton matrix cell row col)
  (let ([singleton (singleton-value cell)])
    (amend-row matrix row singleton)
    (amend-column matrix col singleton)
    (amend-grid matrix (which-grid row col) singleton)))

;; Find a cell containing a singleton set (a set containing just one number) in a given row.
;; For every other set in the same row, the same column, or the same 3x3 box, remove that number (if present).
(define (singleton-search-row matrix list row)
  (let loop ([my-list list]
             [index 0])
    (cond [(empty? my-list) #f]
          [(singleton (first my-list)) (remove-singleton matrix (first my-list) row index)]
          [else (loop (rest my-list) (add1 index))])))

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
    
;; pre-defined rows, columns, matrices and grids for use in testing
    
(define poss-matrix (transform unsolved))

(define first-row (first poss-matrix))
(define third-row (third poss-matrix))
(define test-row (sixth poss-matrix))
(define last-row (ninth poss-matrix))
(define reduced-row-3 (reduce-row-choices poss-matrix 8 3))
(define reduced-row-6 (reduce-row-choices poss-matrix 8 6))

(define sixth-column (get-column poss-matrix 5))
(define ninth-column (get-column poss-matrix 8))
(define third-column (get-column poss-matrix 2))
(define reduced-col-3 (reduce-column-choices poss-matrix 2 3))
(define reduced-col-7 (reduce-column-choices poss-matrix 2 7))

(define first-grid (grid-cell-list poss-matrix 1))
(define ninth-grid (grid-cell-list poss-matrix 9))
(define fourth-grid (grid-cell-list poss-matrix 4))
(define reduced-grid-3 (reduce-grid-choices poss-matrix 4 3))
(define reduced-grid-7 (reduce-grid-choices poss-matrix 4 7))
                  

(provide unsolved
         possible
         poss-matrix
         first-row
         third-row
         test-row 
         last-row
         sixth-column
         ninth-column
         third-column
         first-grid
         ninth-grid
         fourth-grid
         square
         which-grid
         make-set
         list-possible
         transform
         replace
         extract
         ;; amend-set
         ;; remove-singleton-val
         get-row
         get-column
         grid-cell-list
         singleton
         singleton-list
         singleton-value
         list-index
         remove-from-list
         rec-reduce-choices
         reduce-row-choices
         reduce-column-choices
         reduce-grid-choices
         amend-row
         amend-column
         top-grid-row
         left-grid-col
         amend-grid)

    
       
      
