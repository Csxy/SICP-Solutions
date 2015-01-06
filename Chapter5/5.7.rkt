#lang planet neil/sicp
;a)
(define expt-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '* *) (list '- -))
   '((assign continue (label expt-done))
   expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label expt-loop))
   after-expt
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
   base-case
   (assign val (const 1))
   (goto (reg continue))
   expt-done)))
(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 5)
(start expt-machine)
;done
;done
;done
(get-register-contents expt-machine 'val)
;32

;b)
(define expt-machine
  (make-machine
   '(counter product n b)
   (list (list '= =) (list '- -) (list '* *))
   '((assign counter (reg n))
   (assign product (const 1))
   expt
   (test (op =) (reg counter) (const 0))
   (branch (label expt-done))
   (assign counter (op -) (reg counter) (const 1))
   (assign product (op *) (reg b) (reg product))
   (goto (label expt))
   expt-done)))
(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 5)
(start expt-machine)
;done
;done
;done
(get-register-contents expt-machine 'product)
;32