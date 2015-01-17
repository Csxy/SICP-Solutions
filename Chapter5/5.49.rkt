#lang planet neil/sicp
(define test
  '((branch (label external-entry))
    read-eval-print-loop
    (perform
     (op prompt-for-input) (const ";;; EC-Eval input:"))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (perform (op compile-and-run) (reg exp))
    print-result
    (perform (op print-stack-statistics))
    (perform
     (op announce-output) (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))
     external-entry
    (perform (op initialize-stack))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (reg val))))