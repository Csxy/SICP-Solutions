#lang planet neil/sicp

(
 (assign continue (label done))
 eval-dispatch
 (test (op self-evaluating?) (reg exp))
 (branch (label ev-self-eval))
 (test (op variable?) (reg exp))
 (branch (label ev-variable))
 (test (op quoted?) (reg exp))
 (branch (label ev-quoted))
 (test (op assignment?) (reg exp))
 (branch (label ev-assignment))
 (test (op definition?) (reg exp))
 (branch (label ev-definition))
 (test (op if?) (reg exp))
 (branch (label ev-if))
 (test (op lambda?) (reg exp))
 (branch (label ev-lambda))
 (test (op begin?) (reg exp))
 (branch (label ev-begin))
 (test (op application?) (reg exp))
 (branch (label ev-application))
 (goto (label unknown-expression-type))
 
 actual-value
 (save continue)
 (assign continue (label after-eval))
 (goto (label eval-dispatch))
 after-eval
 (restore continue)
 (assign exp (reg val))
 (goto (label force-it))
 
 force-it
 (test (op thunk?) (reg exp))
 (branch (label force-thunk))
 (goto (reg continue))
 force-thunk
 (assign env (op thunk-env) (reg exp))
 (assign exp (op thunk-exp) (reg exp))
 (goto (label actual-value))
 delay-it
 (assign val (op list) (const thunk) (reg exp) (reg env))
 (goto (reg continue))
 
 ;Simple expressions
 
 ev-self-eval
 (assign val (reg exp))
 (goto (reg continue))
 ev-variable
 (assign val (op lookup-variable-value) (reg exp) (reg env))
 (goto (reg continue))
 ev-quoted
 (assign val (op text-of-quotation) (reg exp))
 (goto (reg continue))
 ev-lambda
 (assign unev (op lambda-parameters) (reg exp))
 (assign exp (op lambda-body) (reg exp))
 (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
 (goto (reg continue))
 
 ; Application
 
 ev-application
 (save continue)
 (save env)
 (assign unev (op operands) (reg exp))
 (save unev)
 (assign exp (op operator) (reg exp))
 (assign continue (label ev-appl-did-operator))
 (goto (label actual-value))
 ev-appl-did-operator
 (restore unev)
 (restore env)
 (assign proc (reg val))
 (goto (label apply-dispatch))
 
 
 apply-dispatch
 (test (op primitive-procedure?) (reg proc))
 (branch (label primitive-apply))
 (test (op compound-procedure?) (reg proc))
 (branch (label compound-apply))
 (goto (label unknown-procedure-type))
 primitive-apply
 (assign continue (label primitive-apply-after))
 (assign sign (label actual-value))
 (goto (label list-of-arg-values))
 primitive-apply-after
 (assign val (op apply-primitive-procedure)
         (reg proc)
         (reg argl))
 (restore continue)
 (goto (reg continue))
 
 compound-apply
 (assign continue (label compound-apply-after))
 (assign sign (label delay-it))
 (goto (label list-of-arg-values))
 
 compound-apply-after
 (assign unev (op procedure-parameters) (reg proc))
 (assign env (op procedure-environment) (reg proc))
 (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
 (assign unev (op procedure-body) (reg proc))
 (goto (label ev-sequence))
 
 
 list-of-arg-values
 (assign argl (op empty-arglist))
 (test (op no-operands?) (reg unev))
 (branch (label no-operands))
 (save proc)
 
 ev-appl-operand-loop
 (save argl)
 (assign exp (op first-operand) (reg unev))
 (test (op last-operand?) (reg unev))
 (branch (label ev-appl-last-arg))
 (save env)
 (save unev)
 (save continue)
 (assign continue (label ev-appl-accumulate-arg))
 (save sign)
 (goto (reg sign))
 ev-appl-accumulate-arg
 (restore sign)
 (restore continue)
 (restore unev)
 (restore env)
 (restore argl)
 (assign argl (op adjoin-arg) (reg val) (reg argl))
 (assign unev (op rest-operands) (reg unev))
 (goto (label ev-appl-operand-loop))
 
 ev-appl-last-arg
 (save continue)
 (assign continue (label ev-appl-accum-last-arg))
 (save sign)
 (goto (reg sign))
 ev-appl-accum-last-arg
 (restore sign)
 (restore continue)
 (restore argl)
 (assign argl (op adjoin-arg) (reg val) (reg argl))
 (restore proc)
 (goto (reg continue))
 no-operands
 (goto (reg continue))
 
 ; Sequence
 
 ev-begin
 (assign unev (op begin-actions) (reg exp))
 (save continue)
 (goto (label ev-sequence))
 ev-sequence
 (assign exp (op first-exp) (reg unev))
 (test (op last-exp?) (reg unev))
 (branch (label ev-sequence-last-exp))
 (save unev)
 (save env)
 (assign continue (label ev-sequence-continue))
 (goto (label eval-dispatch))
 ev-sequence-continue
 (restore env)
 (restore unev)
 (assign unev (op rest-exps) (reg unev))
 (goto (label ev-sequence))
 ev-sequence-last-exp
 (restore continue)
 (goto (label eval-dispatch))
 
 ; If
 
 ev-if
 (save exp)
 (save env)
 (save continue)
 (assign continue (label ev-if-decide))
 (assign exp (op if-predicate) (reg exp))
 (goto (label actual-value))
 ev-if-decide
 (restore continue)
 (restore env)
 (restore exp)
 (test (op true?) (reg val))
 (branch (label ev-if-consequent))
 ev-if-alternative
 (assign exp (op if-alternative) (reg exp))
 (goto (label eval-dispatch))
 ev-if-consequent
 (assign exp (op if-consequent) (reg exp))
 (goto (label eval-dispatch))
 
 ; Assignments and definitions
 
 ev-assignment
 (assign unev (op assignment-variable) (reg exp))
 (save unev)
 (assign exp (op assignment-value) (reg exp))
 (save env)
 (save continue)
 (assign continue (label ev-assignment-1))
 (goto (label eval-dispatch))
 ev-assignment-1
 (restore continue)
 (restore env)
 (restore unev)
 (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
 (assign val (const ok))
 (goto (reg continue))
 
 ev-definition
 (assign unev (op definition-variable) (reg exp))
 (save unev)
 (assign exp (op definition-value) (reg exp))
 (save env)
 (save continue)
 (assign continue (label ev-definition-1))
 (goto (label eval-dispatch))
 ev-definition-1
 (restore continue)
 (restore env)
 (restore unev)
 (perform (op define-variable!) (reg unev) (reg val) (reg env))
 (assign val (const ok))
 (goto (reg continue))
 unknown-expression-type
 (assign val (const unknown-expression-type-error))
 (goto (label signal-error))
 unknown-procedure-type
 (restore continue)
 (assign val (const unknown-procedure-type-error))
 (goto (label signal-error))
 signal-error
 (perform (op user-print) (reg val))
 (goto (label read-eval-print-loop))

 unknown-expression-type
 unknown-procedure-type
 done)

