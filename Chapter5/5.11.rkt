#lang planet neil/sicp
;a)
;use 
(restore n) 
;replace:
(assign n (reg val)) 
(restore val) 

;b)
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((pop-reg (pop stack)))
        (if (eq? (car pop-reg) (stack-inst-reg-name inst))
            (set-contents! reg (cdr pop-reg))
            (error "the value is not form order register:" reg-name)))
      (advance-pc pc))))

;c)
(define (make-register name)
  (let ((contents '*unassigned*)
        (stack (make-stack)))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'push)
             ((stack 'push) contents))
            ((eq? message 'pop)  
             (set! contents (stack 'pop))) 
            ((eq? message 'initialize)
             (stack 'initialize))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (reg 'push)
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (reg 'pop)
      (advance-pc pc))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        ;(stack (make-stack))
        (the-instruction-sequence '()))
    (let ((register-table
           (list (list 'pc pc) (list 'flag flag))))
      (let ((the-ops
             (list (list 'initialize-stack
                         (lambda ()
                           (map
                            (lambda (s) ((cadr s) 'initialize))
                            register-table))))))
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
                ((eq? message 'operations) the-ops)
                (else (error "Unknown request -- MACHINE" message))))
        dispatch))))
;remove the stack parameters