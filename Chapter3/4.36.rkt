#lang planet neil/sicp

(define (a-pythagorean-triple-from num)
  (let ((k (an-integer-starting-form num)))
    (let ((j (an-integer-between num k)))
      (let ((i (an-integer-between num j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))