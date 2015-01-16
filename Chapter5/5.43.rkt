#lang planet neil/sicp

(define (lambda-body exp) 
  (scan-out-defines (cddr exp)))