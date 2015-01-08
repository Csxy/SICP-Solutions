#lang planet neil/sicp
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)
                  'start))

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
                            next-inst)
            (extract-labels (cdr text)
                            (lambda (insts labels)
                              (receive (cons (make-instruction label next-inst)
                                             insts)
                                       labels))
                            label)))))
(define (instruction-label inst)
  (car inst))
(define (make-instruction label text)
  (list label text))
(define (instruction-text inst)
  (cadr inst))
(define (instruction-execution-proc inst)
  (cddr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! (cdr inst) proc))
; in (make-new-machine)
(define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if trace-status                                         ;
                    (begin (display (instruction-label (car insts)))
                           (display " - " )
                           (display (instruction-text (car insts)))      ;5.16
                           (newline)))                                   ;
                ((instruction-execution-proc (car insts)))
                (set! insts-number (+ insts-number 1))        ;5.15
                (execute)))))