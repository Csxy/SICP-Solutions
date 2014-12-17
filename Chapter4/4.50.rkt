#lang planet neil/sicp

;in (analyze exp)
 ((ramb? exp) (analyze-ramb exp))

;else
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (define (select-ref n the-list)
    (define (helper count prev-list rest-list)
      (if (= count 0)
          (cons (car rest-list)
                (append prev-list (cdr rest-list)))
          (helper (- count 1)
                  (append prev-list
                          (list (car rest-list)))
                  (cdr rest-list))))
    (helper n '() the-list)) 
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((random-select (select-ref (random (length choices)) 
                                            choices)))
              ((car random-select) env
                                   succeed
                                   (lambda ()
                                     (try-next (cdr random-select)))))))
      (try-next cprocs))))