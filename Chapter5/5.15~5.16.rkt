#lang planet neil/sicp
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (insts-number 0)    ;;;;;;;;;;;;;;5.15
        (trace-status true))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if trace-status                                         ;
                    (begin (display (instruction-text (car insts)))      ;5.16
                           (newline)))                                   ;
                ((instruction-execution-proc (car insts)))
                (set! insts-number (+ insts-number 1))        ;5.15
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-insts-number)               ;;;;;
               (display insts-number)                        ;;;;;5.15
               (set! insts-number 0))                        ;;;;;
              ((eq? message 'trace-on)                               ;
               (set! trace-status true))                             ;
              ((eq? message 'trace-on)                               ; 5.16
               (set! trace-status false))                            ;
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))