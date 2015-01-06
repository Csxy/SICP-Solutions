#lang planet neil/sicp
;(assign val '+ (reg a) (reg b))
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'quote)))