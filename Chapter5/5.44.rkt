#lang planet neil/sicp

(define (compile-open-coded exp target linkage compile-time-env) 
  (let ((proc (car exp))
        (args (cdr exp)))
    (let ((result (find-variable proc compile-time-env)))
      (if (eq? result 'not-found)
          (if (null? (cddr args))
              (end-with-linkage linkage
                                (append-instruction-sequences
                                 (spread-arguments args
                                                   (make-instruction-sequence '(arg1 arg2) (list target)
                                                                              `((assign ,target (op ,proc) (reg arg1) (reg arg2))))
                                                   compile-time-env)))
              (compile (cons proc
                             (cons (list proc
                                         (car args)
                                         (cadr args))
                                   (cddr args)))
                       target
                       linkage
                       compile-time-env))
          (compile-application exp target linkage compile-time-env)))))