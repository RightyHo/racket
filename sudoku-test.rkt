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
      (check = (car (car output)) (seteq 1 2 3 4 5 6 7 8 9))
      (check-equal? (second (car poss-matrix)) (seteq 2))
      (check-equal? (car (second poss-matrix)) (seteq 1))))
   
   ;; poss-matrix test
   
   ;; test-row test
   
   ;; replace test
   
   ;; extract test
   
   ;; amend-set test
   
   ;; remove-singleton-val test
   ;   (test-case
   ;    "Test that the output from function remove-singleton-val is correct"
   ;    (let* ([mtx unsolved]
   ;          (outputlist (remove-singleton-val mtx row column sublist-size init-val single)))
   ;      (check = (length lst) 9)
   ;      (check = (length (first lst)) 9)
   ;      (check = (length outputlist) 9)
   ;      (check = (length (first outputlist)) 9)
   ;      (check-equal? (first (first outputlist)) (seteq 1 2 3 4 5 6 7 8 9))
   ;      (check-equal? (first (second outputlist)) (seteq 1))
   ;      (check-equal? (sixth (third outputlist)) (seteq 4))
   ;      (check-equal? (fourth (fourth outputlist)) (seteq 1 2 3 4 5 6 7 8 9))
   ;      (check-equal? (fifth (fifth outputlist)) (seteq 2))
   ;      (check-equal? (eighth (sixth outputlist)) (seteq 6))))      
   
   ;   (check-equal? (list-index '(1 2 3 4 5 6) 5) 4 "Find index of element in list")
   
   
   ;; get-row test
   
   ;; get-column test
   
   ;; grid-cell-list test
   
   ;; singleton test
   
   ;; singleton-list test 
   
   ;; singleton-value test
   
   ;; list-index test
   
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
