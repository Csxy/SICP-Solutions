#lang planet neil/sicp
ev-application

(save continue)
(assign unev (op operands) (reg exp))
(assign exp (op operator) (reg exp))
(test (op symbol?) (reg exp))           ;symbol?
(branch (label simple-operator))
(save env)
(save unev)
(assign continue (label ev-appl-did-operator))
(goto (label eval-dispatch))

simple-operator
(assign continue (label ev-appl-did-operator-no-restore))
(goto (label eval-dispatch))

ev-appl-did-operator
(restore unev)
(restore env)
ev-appl-did-operator-no-restore
(assign argl (op empty-arglist)) 
(assign proc (reg val))   
(test (op no-operands?) (reg unev)) 
(branch (label apply-dispatch)) 
(save proc) 