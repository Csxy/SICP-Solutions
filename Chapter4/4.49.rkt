#lang planet neil/sicp

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (set! *unparsed* (cdr *unparsed*))
  (apply amb (cdr word-list)))