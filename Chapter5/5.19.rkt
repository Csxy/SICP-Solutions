#lang planet neil/sicp
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (delete-in-set x set)
  (cond ((null? set) (error "can't find the element in set" x))
        ((equal? x (car set)) (cdr set))
        (else (cons (car set)
                    (delete-in-set x (cdr set))))))

;change 
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (trace-status true)
        (breakpoints '()))                       ;***
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
                ((instruction-execution-proc (car insts)))
                (let ((insts (get-contents pc)))                                            ;***
                  (if (null? insts)                                                         ;***
                      'done                                                                 ;***
                      (let ((next-label-line (instruction-label (car insts))))              ;***   
                        (if (element-of-set? next-label-line breakpoints)                   ;***
                            (display next-label-line)                                       ;***
                            (execute)))))))))                                               ;***
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
    
              ((eq? message 'set-breakpoint)                                             ;***
               (lambda (label n)                                                         ;***
                 (set! breakpoints (cons (cons label n) breakpoints))))                  ;***
              ((eq? message 'proceed-machine)                                            ;***
               (execute))                                                                ;***
              ((eq? message 'cancel-breakpoint)                                          ;***
               (lambda (label n)                                                         ;***
                 (set! breakpoints (delete-in-set (cons label n) breakpoints))))         ;***
              ((eq? message 'cancel-all-breakpoint)                                      ;***
               (set! breakpoints '()))                                                   ;***
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)
                  (cons 'start 1)))                                             ;***

(define (extract-labels text receive label)
  (if (null? text)
      (receive '() '())
      (let ((next-inst (car text)))
        (if (symbol? next-inst)
            (extract-labels (cdr text)
                            (lambda (insts labels)
                              (receive insts
                                       (cons (make-label-entry next-inst
                                                               insts)
                                             labels)))
                            (cons next-inst 1))                            ;***
            (extract-labels (cdr text)
                            (lambda (insts labels)
                              (receive (cons (make-instruction label next-inst)
                                             insts)
                                       labels))
                            (cons (car label)
                                  (+ (cdr label) 1)))))))                       ;***


(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (proceed-machine machine)
  (machine 'proceed-machine))
(define (cancel-breakpoint label n)
  ((machine 'cancel-breakpoint) label n))
(define (cancel-all-breakpoint)
  (machine 'cancel-all-breakpoint))