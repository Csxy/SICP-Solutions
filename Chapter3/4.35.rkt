#lang planet neil/sicp
(define (an-integer-between low high)
  (amb low (an-integer-between (+ low 1) high)))