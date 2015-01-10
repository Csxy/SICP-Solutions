#lang planet neil/sicp
;a)
(controller
 (assign continue (label append-done))
 append-loop
 (test (op null?) (reg x))
 (branch (label base-case))
 (save continue)
 (assign continue (label after-append))
 (save x)
 (assign x (op cdr) (reg x))
 (goto (label append-loop))
 after-append
 (restore x)
 (assign x (op car) (reg x))
 (assign val (op cons) (reg x) (reg val))
 (restore continue)
 (goto (reg continue))
 base-case
 (assign val (reg y))
 (goto (reg continue))
 append-done)

;b)
(controller
 (save x)
 last-pair-loop
 (assign temp (op cdr) (reg x))
 (test (op null?) (reg temp))
 (branch (label last-pair-done))
 (assign x (reg temp))
 (goto (label last-pair-loop))
 last-pair-done
 (perform (op set-cdr!) (reg x) (reg y))
 (restore x))
 
 