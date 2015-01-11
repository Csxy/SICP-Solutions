#lang planet neil/sicp

eval-dispatch
(test (op cond?) (reg exp))
(branch (label ev-cond))
(test (op let?) (reg exp))
(branch (label ev-let))



ev-cond
(assign exp (op cond->if) (reg exp))
(goto (label ev-if))

ev-let
(assign exp (op let->lambda) (reg exp))
(goto (label ev-lambda))

