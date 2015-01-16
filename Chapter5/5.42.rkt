#lang planet neil/sicp
(define (compile-variable exp target linkage compile-time-env)
  (let ((address (find-variable exp compile-time-env)))
    (end-with-linkage linkage
                      (if (eq? address 'not-found)
                          (make-instruction-sequence '(env) (list target)
                                                     `((save env)
                                                       (assign env (op get-global-environment))
                                                       (assign ,target
                                                         (op lookup-variable-value)
                                                         (const ,exp)
                                                         (reg env))
                                                       (restore env)))
                          (make-instruction-sequence '(env) (list target)
                                                     `((assign ,target
                                                               (op lexical-address-lookup)
                                                               (const ,address)
                                                               (reg env))))))))
  
(define (compile-assignment exp target linkage compile-time-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next compile-time-env)))
    (let ((address (find-variable exp compile-time-env)))
      (end-with-linkage linkage
                        (preserving '(env)
                                    get-value-code
                                    (if (eq? address 'not-found)
                                        (make-instruction-sequence '(env) (list target)
                                                                   `((save env)
                                                                     (assign env (op get-global-environment))
                                                                     (perform (op set-variable-value!)
                                                                              (const ,var)
                                                                              (reg val)
                                                                              (reg env))
                                                                     (restore env)))
                                        
                                        (make-instruction-sequence '(env val) (list target)
                                                                   `((perform (op lexical-address-set!)
                                                                              (const ,address)
                                                                              (reg val)
                                                                              (reg env))
                                                                     (assign ,target (const ok))))))))))