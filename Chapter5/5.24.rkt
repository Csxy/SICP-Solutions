#lang planet neil/sicp

eval-dispatch
(test (op cond?) (reg exp))
(branch (label ev-cond))

ev-cond
(assign unev (op cond-clauses) (reg exp))
(save env)
(save continue)

cond-loop
(test (op null?) (reg unev))
(branch (label cond-no-more))
(assign exp (op car) (reg unev))
(test (op else-cause?) (reg exp))
(branch (label cond-end))
(save unev)
(save exp)
(assign continue (label cond-decide))
(assign exp (op cond-predicate) (reg exp))
(goto (label eval-dispatch))
cond-decide
(restore exp)
(restore unev)
(test (op true?) (reg val))
(branch (label cond-end))
(assign unev (op cdr) (reg unev))
(goto (label cond-loop))
cond-end
(restore continue)
(restore env)
(assign exp (op cond-actions) (reg exp))
(goto (label eval-dispatch))
cond-no-more
(restore continue)
(restore env)
(goto (reg continue))

