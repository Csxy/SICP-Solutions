#lang planet neil/sicp
(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on true))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace-on
                   (begin (display name)
                          (display ":")
                          (display contents)
                          (display "->")
                          (display value)
                          (newline)))
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! trace-on true))
            ((eq? message 'trace-off)
             (set! trace-on false))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))
;change in (make-new-machine)
(define (dispatch message)
  ;......
  ;......
  ((eq? message 'trace-on-reg)
   (lambda (reg) ((lookup-register reg) 'trace-on)))
  ((eq? message 'trace-off-reg)
   (lambda (reg) ((lookup-register reg) 'trace-off)))
  ;....