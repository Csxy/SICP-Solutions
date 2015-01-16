#lang planet neil/sicp
(define (lexical-address-lookup address env)
  (let ((the-frame (list-ref env (car address))))
    (let ((the-value (list-ref (frame-values the-frame)
                               (cadr address))))
      (if (eq? the-value '*unassigned*)
          (error "The variable is undefined.")
          the-value))))

(define (lexical-address-set! address new-value env)
  (define (var-set vals number)
    (if (= number 0)
        (set-car! vals new-value)
        (var-set (cdr vals) new-value)))
  (let ((the-frame (list-ref env (car address))))
    (var-set (frame-values the-frame)
             (cadr address))))

