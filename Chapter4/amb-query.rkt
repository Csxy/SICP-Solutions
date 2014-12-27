#lang planet neil/sicp
;==== for 4.78
(define (applyn fproc aprocs)
  (apply-primitive-procedure fproc aprocs))
(define (evaln var)
  (lookup-variable-value var the-global-environment))

(define (amb-result? exp)
  (tagged-list? exp 'amb-result))
(define (analyze-amb-result exp)
  (lambda (env succeed fail)
    ((analyze (cadr exp)) env 
                          (lambda (var fail2)
                            (succeed true fail))
                          (lambda ()
                            (succeed false fail)))))
;=====
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-set? exp) (analyze-permanent-set exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((amb-result? exp) (analyze-amb-result exp)) ;for 4.78
        ((ramb? exp) (analyze-ramb exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ;((let*? exp) (eval (let*->nested-lets exp) env))
        ;((letrec? exp) (eval (letrec->let exp) env))
        ;((for? exp) (eval (for->let exp) env)) ;4.9
        ;((clean? exp) (delete-var exp env)) 
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unkonwn procedure type -- EXECUTE-APPLICATION"
          proc))))

;----------4.3.3---------
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;---------------------------
;4.50
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (define (select-ref n the-list)
    (define (helper count prev-list rest-list)
      (if (= count 0)
          (cons (car rest-list)
                (append prev-list (cdr rest-list)))
          (helper (- count 1)
                  (append prev-list
                          (list (car rest-list)))
                  (cdr rest-list))))
    (helper n '() the-list)) 
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((random-select (select-ref (random (length choices)) 
                                             choices)))
              ((car random-select) env
                                   succeed
                                   (lambda ()
                                     (try-next (cdr random-select)))))))
      (try-next cprocs))))


;---



(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) 
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail) 
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail) 
      (proc1 env 
             (lambda (a-value fail2)
               (proc2 env succeed fail2))
             fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
;----4.51----
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
;-----4.52-------
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
;------------------
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail) (succeed (make-procedure vars bproc env) fail))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))
; 'string
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
; set!
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
; define
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
;lambda 
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
;if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
;begin
(define (begin? exp)  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
;application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-> if"
                       clauses))
            (make-if (cond-predicate first)
                     (if (tagged-list?  (cond-actions first) '=>)
                         (list (cadr (cond-actions first)) (cond-predicate first))
                         (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))
;and or
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and-action exp) (cdr exp))
(define (or-action exp) (cdr exp))
(define (and->if exp)
  (expand-and (and-action exp)))
(define (or->if exp)
  (expand-or (or-action exp)))
(define (expand-and exp)
  (if (null? exp)
      'true
      (if (null? (rest-exps exp))
          (first-exp exp)
          (make-if (first-exp exp)
                   (expand-and (rest-exps exp))
                   'false)))) 
(define (expand-or exp)
  (if (null? exp)
      'false
      (if (null? (rest-exps exp))
          (first-exp exp)
          (make-if (first-exp exp)
                   (first-exp exp)
                   (expand-or (rest-exps exp))))))
;let 
(define (let? exp) (tagged-list? exp 'let))
(define (name-let? exp) (and (let? exp) (symbol? (cadr exp))))
(define (let-parameters exp)
  (if (name-let? exp)
      (map car (caddr exp))
      (map car (cadr exp))))
(define (let-inits exp)
  (if (name-let? exp)
      (map cadr (caddr exp))
      (map cadr (cadr exp))))
(define (let-body exp)
  (if (name-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-name exp)
  (cadr exp))
(define (let->combination exp)
  (if (name-let? exp)
      (sequence->exp (list
                      (list 'define 
                            (let-name exp)
                            (make-lambda (let-parameters exp)
                                         (let-body exp)))
                      (cons (let-name exp) (let-inits exp))))
      (cons (make-lambda (let-parameters exp)
                         (let-body exp))
            (let-inits exp))))
(define (make-let var-exp-list body)
  (cons 'let (cons var-exp-list body)))
;let* 
(define (let*? exp) (tagged-list? exp 'let*)) 
(define (let-varlist exp) (cadr exp))
(define (let*->nested-lets exp)
  (define (helper var-exp-list body)
    (if (null? var-exp-list)
        body
        (list (make-let (list (car var-exp-list))
                        (helper (cdr var-exp-list) body)))))
  (car (helper (let-varlist exp) (let-body exp))))
;for
(define (for? exp) (tagged-list? exp 'for))
(define (for-1st exp) (caadr exp))
(define (for-2nd exp) (cadadr exp))
(define (for-3rd exp) (cddadr exp))
(define (for-body exp) (cddr exp))
(define (for->let exp)
  (make-let (list (for-1st exp))
            (list (list 'begin (list 'define '(f) 
                                     (make-if (for-2nd exp)
                                              (list 'begin 
                                                    (sequence->exp (for-body exp))
                                                    (sequence->exp (for-3rd exp))
                                                    '(f))
                                              ''ok))
                        '(f)))))

; true and false
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))
;4.16b
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (scan-out-defines body)
  (let ((the-difinitions (filter (lambda (exp) (definition? exp)) body)))
    (if (null? the-difinitions)
        body
        (list (make-let (map (lambda (exp) (list (definition-variable exp) 
                                                 ''*unassigned*))
                             the-difinitions)
                        (append (map (lambda (x) 
                                       (list 'set! (definition-variable x) (definition-value x))) 
                                     the-difinitions)
                                (filter (lambda (exp) (not (definition? exp))) body)))))))
;4.20
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let body)
  (make-let (map (lambda (x) (list x ''*unassigned*)) (let-parameters body))
            (append (map (lambda (x y) (list 'set! x y)) (let-parameters body) (let-inits body))
                    (let-body body))))

;process
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;frame
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;(define (make-frame variables values)
;  (cons variables values))
(define (make-binding variable value)
  (cons variable value))

;(define (frame-variables frame) (car frame))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
;(define (frame-values frame) (cdr frame))
;(define (add-binding-to-frame! var val frame)
;  (set-car! frame (cons var (car frame)))
;  (set-cdr! frame (cons val (cdr frame))))
(define (add-binding-to-frame! var val frame)
  (cons (make-binding var val) frame))

;(define (extend-environment vars vals base-env)
;  (if (= (length vars) (length vals))
;      (cons (make-frame vars vals) base-env)
;      (if (< (length vars) (length vals))
;          (error "Too many arguments supplied" vars vals)
;          (error "Too few arguments supplied" vars vals))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (map make-binding vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; (define (lookup-variable-value var env)
;   (define (env-loop env)
;     (define (scan vars vals)
;       (cond ((null? vars)
;              (env-loop (enclosing-environment env)))
;             ((eq? var (car vars))
;              (car vals))
;             (else (scan (cdr vars) (cdr vals)))))
;     (if (eq? env the-empty-environment)
;         (error "Unbound variable" var)
;         (let ((frame (first-frame env)))
;           (scan (frame-variables frame)
;                 (frame-values frame)))))
;   (env-loop env))

;4.11
; (define (lookup-variable-value var env)
;   (define (env-loop env)
;     (define (scan frame)
;       (cond ((null? frame)
;              (env-loop (enclosing-environment env)))
;             ((eq? var (binding-variable (car frame)))
;              (binding-value (car frame)))
;             (else (scan (cdr frame)))))
;     (if (eq? env the-empty-environment)
;         (error "Unbound variable" var)
;         (scan (first-frame env))))
;   (env-loop env))

;(define (set-variable-value var val env)
;  (define (env-loop env)
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop (enclosing-environment env)))
;            ((eq? var (car vars))
;             (set-car! vals val))
;            (else (scan (cdr vars) (cdr vals)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable -- SET!" var)
;        (let ((frame (first-frame env)))
;          (scan (frame-variables frame)
;                (frame-values frame)))))
;  (env-loop env))

;4.11
; (define (set-variable-value var val env)
;   (define (env-loop env)
;     (define (scan frame)
;       (cond ((null? vars)
;              (env-loop (enclosing-environment env)))
;             ((eq? var (binding-variable (car frame)))
;              (set-cdr! (car frame) val))
;             (else (scan (cdr frame)))))
;     (if (eq? env the-empty-environment)
;         (error "Unbound variable -- SET!" var)
;         (scan (first-frame env))))
;   (env-loop env))


; (define (define-variable! var val env)
;   (let ((frame (first-frame env)))
;     (define (scan vars vals)
;       (cond ((null? vars)
;              (add-binding-to-frame! var val frame))
;             ((eq? var (car vars))
;              (set-car! vals val))
;             (else (scan (cdr vars) (cdr vals)))))
;     (scan (frame-variables frame)
;           (frame-values frame))))

;4.11
; (define (define-variable! var val env)
;     (define (scan frame
;       (cond ((null? vars)
;              (add-binding-to-frame! var val frame))
;             ((eq? var (binding-variable (car frame)))
;              (set-cdr! (car frame) val))
;             (else (scan (cdr frame)))))
;     (scan (first-frame env))))

;4.12
(define (scan var frame)
  (cond ((null? frame)
         false)
        ((eq? var (binding-variable (car frame)))
         (car frame))
        (else (scan var (cdr frame)))))
(define (env-loop var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((result (scan var (first-frame env))))
        (if result
            result
            (env-loop var (enclosing-environment env))))))
(define (lookup-variable-value var env)
  (let ((re (binding-value (env-loop var env))))
    (if (eq? re '*unassigned*)
        (error "unassigned ---LOOKUP-VAR" var)
        re)))
(define (set-variable-value! var val env)
  (set-cdr! (env-loop var env) val))
(define (define-variable! var val env)
  (let ((result (scan var (first-frame env))))
    (if result
        (set-cdr! result val)
        (set-car! env (add-binding-to-frame! var val (first-frame env))))))
;4.13
(define (clean? exp)
  (tagged-list? exp 'clean!))
(define (delete-var exp env)
  (define (env-loop var env)
    (define (scan var frame)
      (cond ((null? (cdr frame))
             (env-loop (enclosing-environment env)))
            ((eq? var (binding-variable (cadr frame)))
             (begin (set-cdr! frame (cddr frame))
                    'ok))
            (else (scan var (cdr frame)))))
    (if (eq? env the-empty-environment) 
        (error "Unbound variable -- SET!" var)
        (if (eq? var (binding-variable (car (first-frame env))))
            (begin (set-car! (first-frame env) (cdr (first-frame env)))
                   (set-cdr! (first-frame env) (cddr (first-frame env)))
                   'ok)
            (let ((result (scan var (first-frame env))))
              (if result
                  result
                  (env-loop var (enclosing-environment env)))))))
  (env-loop (cadr exp) env))


(define (user-initial-environment) the-global-environment)
;primitive
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'caddr caddr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'assoc assoc)
        (list 'not not)
        (list 'number? number?)
        (list 'display display)
        (list '+ +)
        (list '* *)
        (list '= =)
        (list '- -)
        (list '< <)
        (list '> >)
        (list 'newline newline)
        (list 'list list)
        (list 'member member)
        (list 'abs abs)
        (list 'equal? equal?)
        (list 'eq? eq?)
        (list 'even? even?)
        (list 'memq memq)
        (list 'var? 'var?)
        (list 'symbol? symbol?)
        (list 'string->symbol string->symbol)
        (list 'string-append string-append)
        (list 'symbol->string symbol->string)
        (list 'number->string number->string)
        (list 'substring substring)
        (list 'string=? string=?)
        (list 'set-cdr! set-cdr!)
        (list 'set-car! set-car!)
        (list 'pair? pair?)
        (list 'string-length string-length)
        (list 'append append)
        (list 'apply applyn)
        (list 'eval evaln)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
;4.1.4
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env) 
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(driver-loop)
