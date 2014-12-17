#lang planet neil/sicp

;in (analyze exp)
((if-fail? exp) (analyze-if-fail exp))
;else
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
  
(define (analyze-if-fail exp)
  (let ((if-succeed (analyze (cadr exp)))
        (if-fail (analyze (caddr exp))))
    (lambda (env succeed fail)
      (if-succeed env
                  succeed
                  (lambda ()
                    (if-fail env succeed fail))))))