#lang planet neil/sicp
;new conjoin
(define (conjoin conjuncts frame-stream)
  (if (procedure? frame-stream)
      (frame-stream conjoin conjuncts)
      (if (empty-conjunction? conjuncts)
          frame-stream
          (conjoin (rest-conjuncts conjuncts)
                   (qeval (first-conjunct conjuncts)
                          frame-stream)))))
;new negate
(define (negate operands frame-stream)
  (lambda (f contents)
    (let ((new-frame-stream (f contents frame-stream)))
      (stream-flatmap
       (lambda (frame)
         (if (stream-null? (qeval (negated-query operands)
                                  (singleton-stream frame)))
             (singleton-stream frame)
             the-empty-stream))
       new-frame-stream))))
;new lisp-value
(define (lisp-value call frame-stream)
  (lambda (f contents)
    (let ((new-call (f contents frame-stream)))
      (stream-flatmap
       (lambda (frame)
         (if (execute
              (instantiate
                  new-call
                frame
                (lambda (v f)
                  (error "Unknown pat var -- LISP-VALUE" v))))
             (singleton-stream frame)
             the-empty-stream))
       frame-stream))))
;input 
(and (not (job ?x (computer programmer)))
    (supervisor ?x ?y))
;output
(and (not (job (aull dewitt) (computer programmer))) (supervisor (aull dewitt) (warbucks oliver)))
(and (not (job (cratchet robert) (computer programmer))) (supervisor (cratchet robert) (scrooge eben)))
(and (not (job (scrooge eben) (computer programmer))) (supervisor (scrooge eben) (warbucks oliver)))
(and (not (job (bitdiddle ben) (computer programmer))) (supervisor (bitdiddle ben) (warbucks oliver)))
(and (not (job (reasoner louis) (computer programmer))) (supervisor (reasoner louis) (hacker alyssa p)))
(and (not (job (tweakit lem e) (computer programmer))) (supervisor (tweakit lem e) (bitdiddle ben)))

