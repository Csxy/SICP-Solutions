#lang planet neil/sicp
;a)
(controller
 (assign continue (label count-done))
 count-loop
 (test (op null?) (reg tree))
 (branch (label cond-1))
 (assign temp (op pair?) (reg tree))
 (test (op not) (reg temp))
 (branch (label cond-2))
 (save continue)
 (assign continue (label after-car))
 (save tree)
 (assign tree (op car) (reg tree))
 (goto (label count-loop))
 after-car
 (restore tree)
 (assign tree (op cdr) (reg tree))
 (assign continue (label after-cdr))
 (save val)
 (goto (label count-loop))
 after-cdr
 (assign temp (reg val))
 (restore val)
 (restore continue)
 (assign val
         (op +) (reg val) (reg temp))
 (goto (reg continue))
 cond-1
 (assign val (const 0))
 (goto (reg continue))
 cond-2
 (assign val (const 1))
 (goto (reg continue))
 count-done)
;b)
(controller
 (assign continue (label iter-done))
 (assign n (const 0))
 iter-loop
 (test (op null?) (reg tree))
 (branch (label cond-1))
 (assign temp (op pair?) (reg tree))
 (test (op not) (reg temp))
 (branch (label cond-2))
 (save continue)
 (assign continue (label after-iter))
 (save tree)
 (assign tree (op car) (reg tree))
 (goto (label iter-loop))
 after-iter
 (restore tree)
 (assign tree (op cdr) (reg tree))
 (restore continue)
 (goto (label iter-loop))
 cond-1
 (goto (reg continue))
 cond-2
 (assign n (op +) (reg n) (const 1))
 (goto (reg continue))
 iter-done)