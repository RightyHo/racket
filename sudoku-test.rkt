#lang racket

(require rackunit
         "sudoku.rkt")

(require rackunit/text-ui)

(define sudoku-tests
  (test-suite
   "Tests for sudoku.rkt"
   
   ;; unsolved test
   (test-case
    "Our 'unsolved' input test list has length 9 and all elements are lists"
    (let ([lst unsolved])
      (check = (length lst) 9)
      (for-each
       (lambda (element)
         (check-pred list? element))
       lst)))
   
   ;; which-grid test
   (check-equal? (which-grid 6 4) 8 "Find grid ID for specific cell on the board")
   
   ;; make-set test
   (check-equal? (make-set 0) (seteq 1 2 3 4 5 6 7 8 9) "Test that set of all possible values is returned")
   
   (check-equal? (make-set 5) (seteq 5) "Test that a set with only one member with the value of 5 is returned")
   
   ;; list-possible test
   (test-case
    "Test that the output from function list-possible is correct"
    (let* ([lst unsolved]
           [outputlist (list-possible lst)])
      (check = (length lst) 9)
      (check = (length (first lst)) 9)
      (check = (length outputlist) 9)
      (check = (length (first outputlist)) 9)
      (check-equal? (first (first outputlist)) (seteq 1 2 3 4 5 6 7 8 9))
      (check-equal? (first (second outputlist)) (seteq 1))
      (check-equal? (sixth (third outputlist)) (seteq 4))
      (check-equal? (fourth (fourth outputlist)) (seteq 1 2 3 4 5 6 7 8 9))
      (check-equal? (fifth (fifth outputlist)) (seteq 2))
      (check-equal? (eighth (sixth outputlist)) (seteq 6))))      
      
   ;; transform test
   (test-case
    "Test that tranform function returns correct output"
    (let ([output (transform unsolved)])
      (check-equal? (car (car output)) (seteq 1 2 3 4 5 6 7 8 9))
      (check-equal? (second (car poss-matrix)) (seteq 2))
      (check-equal? (car (second poss-matrix)) (seteq 1))))
   
   ;; replace test
   (test-case
    "check that repace function performs as expected"
    (let ([output (replace '(3 5 6 7 4 5) 5 000)])
      (check-equal? output '(3 000 6 7 4 000))))

   ;; extract test
   (test-case
    "test that the extract function returns expected output"
    (let ([output (extract unsolved 3 4 3)])
      (check-equal? output '(0 0 3))))
   
   ;; get-row test
   (test-case
    "test the row returned by get-row is the expected outcome"
    (let ([output (get-row unsolved 3)])
      (check-equal? output '(0 5 0 0 0 0 3 2 0))))
   
   ;; get-column test
   (test-case
    "test the column returned by get-column is the expected outcome"
    (let ([output (get-column unsolved 3)])
      (check-equal? output '(0 2 0 0 0 0 5 0 6))))
   
   ;; grid-cell-list test
   (test-case
    "test the grid list returned grid-cell-list contains the expected elements"
    (let ([output (grid-cell-list unsolved 4)])
      (check-equal? output '(0 5 0 6 0 0 0 8 7))))
   
   ;; singleton test
   
   ;; singleton-list test 
   
   ;; singleton-value test
   
   ;; list-index test
      (check-equal? (list-index '(1 2 3 4 5 6) 5) 4 "Find index of element in list")
      
   ;; remove-from-list test
   
   ;; rec-reduce-choices test
   
   ;; reduce-row-choices test
   
   ;; reduce-column-choices test
   
   ;; reduce-grid-choices test
   
   ;; amend-row test
   
   ;; amend-column test
   
   ;; insert-list test
   
   
   
   
   
   
   
   
   
   
   ))

(run-tests sudoku-tests)
