#lang planet neil/sicp
;a)
(define (spread-arguments argl seq)
  (let ((seq1 (compile (car argl) 'arg1 'next))
        (seq2 (compile (cadr argl) 'arg2 'next)))
        (preserving '(env) 
                    seq1 
                    (preserving '(arg1) 
                                seq2
                                seq))))




;b)
(define (compile exp target linkage)
  (cond ((open-coded? exp)
         (compile-open-coded exp target linkage))))


(define (compile-open-coded exp target linkage)
  (end-with-linkage linkage 
                    (append-instruction-sequences
                     (spread-arguments (cdr exp)
                                       (make-instruction-sequence '(arg1 arg2) (list target)
                                                                  `((assign ,target (op ,(car exp)) (reg arg1) (reg arg2))))))))

(define (open-coded? exp)
  (memq (car exp) '(+ - * /)))

;d)

(define (compile-open-coded exp target linkage)
  (let ((proc (car exp))
        (args (cdr exp)))
    (if (null? (cddr args))
        (end-with-linkage linkage
                          (append-instruction-sequences
                           (spread-arguments args
                                             (make-instruction-sequence '(arg1 arg2) (list target)
                                                                        `((assign ,target (op ,proc) (reg arg1) (reg arg2)))))))
        (compile (cons proc
                       (cons (list proc
                                   (car args)
                                   (cadr args))
                             (cddr args)))
                 target
                 linkage))))
                       