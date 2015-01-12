#lang planet neil/sicp
;a)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (cons 'bound (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (cons 'unbound var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (lookup? re)
  (and (pair? re)
       (eq? (car re) 'bound)))
(define (extract-variable-value re)
  (cdr re))


ev-variable
(assign val (op lookup-variable-value) (reg exp) (reg env))
(test (op lookup?) (reg val))
(branch (label bound-variable))
(assign val (op list) (const error-unbound-variable:) (reg exp))
(goto (label signal-error))
bound-variable
(assign val (op extract-variable-value) (reg val)) 
(goto (reg continue))

;b)
(define (safe? val)
  (tagged-list? val 'safe))

(define (div-check proc args)
  (if (= 0 (cadr args))
      (cons 'unsafe 
            "/: division by zero")
      (cons 'safe
            (apply-in-underlying-scheme proc args))))
(define (car-check proc args)
  (if (pair? (car args))
      (cons 'safe
            (apply-in-underlying-scheme proc args))
      (cons 'unsafe
            (list proc "arg-isnot-pair"))))
(define cdr-check car-check)
(define check-list
  (list (list / div-check)
        (list car car-check)
        (list cdr cdr-check)))

(define (apply-primitive-procedure proc args)
  (let ((proc (primitive-implementation proc)))
    (let ((result (assoc proc check-list)))
      (if result
          ((cadr result) proc args)
          (cons 'safe 
                (apply-in-underlying-scheme proc args))))))

primitive-apply
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(test (op safe?) (reg val))
(branch (label safe-primitive))
(goto (label signal-error))
safe-primitive
(restore continue)
(goto (reg continue))
