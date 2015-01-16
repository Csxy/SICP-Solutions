#lang planet neil/sicp
(define (find-variable var env)
  (define (env-loop env frame-number)
    (define (scan frame number)
      (cond ((null? frame) 
             (env-loop (cdr env) (+ frame-number 1)))
            ((eq? var (car frame)) (list frame-number number))
            (else (scan (cdr frame) (+ number 1)))))
    (if (eq? env '())
       'not-found
        (scan (car env) 0)))
  (env-loop env 0))