#lang planet neil/sicp
(define (make-operation-exp expr machine labels operations) 
  (let ((op (lookup-prim (operation-exp-op expr) operations)) 
        (aprocs 
         (map (lambda (e) 
                (if (label-exp? e) 
                    (error "the operands can't be label -- MAKE-OPERATION-EXP" e) 
                    (make-primitive-exp e machine labels))) 
              (operation-exp-operands expr))))
    (lambda () 
      (apply op (map (lambda (p) (p)) aprocs)))))