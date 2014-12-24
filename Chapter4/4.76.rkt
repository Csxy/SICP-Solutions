#lang planet neil/sicp

(define (conjoin conjuncts frame-stream)
  (cond ((empty-conjunction? conjuncts) frame-stream)
        ((empty-conjunction? (rest-conjuncts conjuncts))
         (qeval (first-conjunct conjuncts)
                frame-stream))
        (else (stream-match (qeval (first-conjunct conjuncts)
                                   frame-stream)
                            (conjoin (rest-conjuncts conjuncts) 
                                     frame-stream))))) 

(define (stream-match stream-1 stream-2)
  (stream-flatmap
   (lambda (frame-1)
     (stream-filter (lambda (s) (not (eq? s 'failed)))
                    (stream-map
                     (lambda (frame-2)
                       (frame-match frame-1 frame-2))
                     stream-2)))
   stream-1))


(define (frame-match frame-1 frame-2)
  (define (iter frame result)
    (if (null? frame)
        result
        (let ((first-binding (car frame)))
          (let ((binding (binding-in-frame (binding-variable first-binding)
                                           frame-2)))
            (cond ((not binding) 
                   (iter (cdr frame)
                         (extend (binding-variable first-binding)
                                 (binding-value first-binding)
                                 result)))
                  ((equal? (binding-value binding)
                           (binding-value first-binding))
                   (iter (cdr frame) result))
                  (else 'failed))))))
  (iter frame-1 frame-2))

(put 'and 'qeval conjoin)