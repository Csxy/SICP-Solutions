#lang planet neil/sicp

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine) register-names)
    machine))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-inst inst insts-list)
  (if (null? insts-list) 
      (list (list (car inst)
                  (list inst)))
      (let ((first-list (car insts-list)))
        (if (eq? (car inst) (car first-list))
            (if (element-of-set? inst (cadr first-list))
                insts-list
                (cons (list (car inst)
                            (cons inst
                                  (cadr first-list)))
                      (cdr insts-list)))
            (cons first-list
                  (adjoin-inst inst (cdr insts-list)))))))

(define (make-insts-list insts-seq)
  (if (null? insts-seq)
      '()
      (adjoin-inst (car insts-seq)
                   (make-insts-list (cdr insts-seq)))))

(define (label-regs insts-list)
  (apply append (map
                 (lambda (inst)
                   (if (eq? (caadr inst) 'reg)
                       (list (cadadr inst))
                       '()))
                 (cadr (assoc 'goto insts-list)))))

(define (stack-regs insts-list)
  (define (merger set1 set2)
    (cond ((null? set1) set2)
          ((element-of-set? (car set1) set2)
           (merger (cdr set1) set2))
          (else (cons (car set1)
                      (merger (cdr set1) set2)))))
  (merger 
   (map 
    (lambda (inst)
      (cadr inst))
    (cadr (assoc 'save insts-list)))
   (map
    (lambda (inst)
      (cadr inst))
    (cadr (assoc 'restore insts-list)))))

(define (regs-source regs insts-list)
  (map
   (lambda (reg)
     (cons reg
           (apply
            append
            (map
             (lambda (inst)
               (if (eq? (cadr inst) reg)
                   (list (cddr inst))
                   '()))
             (cadr (assoc 'assign insts-list))))))
   regs))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-list '())
        (label-regs-list '())
        (stack-regs-list '())
        (regs-source-list '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
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
                (execute)))))
      (define (initialize seq regs)
        (set! the-instruction-sequence seq)
        (let ((init-list (make-insts-list (map car seq))))
          (set! instruction-list init-list)
          (set! label-regs-list (label-regs  init-list))
          (set! stack-regs-list (stack-regs init-list))
          (set! regs-source-list (regs-source regs init-list))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               initialize)
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-data) 
               (list instruction-list
                     label-regs-list
                     stack-regs-list 
                     regs-source-list))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


