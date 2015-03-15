#lang racket

(require rackunit
         "sudoku.rkt")

(require rackunit/text-ui)
 
(run-tests sudoku-tests)

(define sudoku-tests
  (test-suite
   "Tests for sudoku.rkt"
 
   (check-equal? (list-index '(1 2 3 4 5 6) 5) 4 "Find index of element in list")
 
   (check-equal? (which-grid 6 4) 8 "Find grid ID for specific cell on the board")
   
   (check-equal? (make-set 0) (seteq 1 2 3 4 5 6 7 8 9) "Test that set of all possible values is returned")
   
   (check-equal? (make-set 5) (seteq 5) "Test that a set with only one member with the value of 5 is returned")
 
   (test-case
    "List has length 9 and all elements are lists"
    (let ([lst '((0 2 5 0 0 1 0 0 0)
                 (1 0 4 2 5 0 0 0 0)
                 (0 0 6 0 0 4 2 1 0)
                 (0 5 0 0 0 0 3 2 0)
                 (6 0 0 0 2 0 0 0 9)
                 (0 8 7 0 0 0 0 6 0)
                 (0 9 1 5 0 0 6 0 0)
                 (0 0 0 0 7 8 1 0 3)
                 (0 0 0 6 0 0 5 9 0))])
      (check = (length lst) 9)
      (for-each
        (lambda (elt)
          (check-pred list? elt))
      lst)))))

