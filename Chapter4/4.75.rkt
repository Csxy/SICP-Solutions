#lang planet neil/sicp

(define (uniquely-asserted query frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result (qeval (negated-query query) (singleton-stream frame))))
       (cond ((stream-null? result) the-empty-stream)
             ((stream-null? (stream-cdr result)) result)
             (else empty-stream))))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)