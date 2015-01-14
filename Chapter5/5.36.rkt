#lang planet neil/sicp
;right to left
(define (construct-arglist operand-codes) 
  (if (null? operand-codes) 
      (make-instruction-sequence '() '(argl) 
                                 `((assign argl (const ())))) 
      (let ((code-to-get-last-arg 
             (append-instruction-sequences 
              (car operand-codes) 
              (make-instruction-sequence '(val) '(argl) 
                                         `((assign argl (op list) (reg val))))))) 
        (if (null? (cdr operand-codes)) 
            code-to-get-last-arg 
            (tack-on-instruction-sequence 
             (preserving '(env) 
                         code-to-get-last-arg 
                         (code-to-get-rest-args 
                          (cdr operand-codes))) 
             (make-instruction-sequence '() '() 
                                        '((assign argl (op reverse) (reg argl)))))))))
