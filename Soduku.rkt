#lang racket

(struct square (grid row col poss-list) #:transparent)

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

(define possible (seteq 1 2 3 4 5 6 7 8 9))

(define (make-set x)
  (if (eq? x 0)
      possible
      (seteq x)))

(define (list-possible unsolved)
  (for/list ([x unsolved])
    (if (list? x)
        (map make-set x)
        (printf "problem: ~a is not a list" x))))
  

      
