#lang planet neil/sicp

;in (analyze exp)
((permanent-set? exp) (analyze-permanent-set exp))

;else
(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-set exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok
                        fail2))
             fail))))