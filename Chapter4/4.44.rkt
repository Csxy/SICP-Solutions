#lang planet neil/sicp

(define (queens board-size)
  (define (queen-cols k positions)
    (if (= k board-size)
        positions
        (let ((new (an-integer-between 1 board-size)))
          (require (safe? new positions))
          (queen-cols (+ k 1) (cons new positions)))))
  (queen-cols 0 '()))

(define (safe? k solution)
  (define (attack? row col)
    (or (= k row)
        (= k (+ row col))
        (= k (- row col))))
  (define (check rest i)
    (cond ((null? rest) true) 
          ((conflict? (car rest) i) false) 
          (else (check (cdr rest) (+ i 1)))))
  (check solution 1))