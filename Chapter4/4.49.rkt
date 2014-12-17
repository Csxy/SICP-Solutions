#lang planet neil/sicp

(define (list-ramb alist) 
   (if (null? alist) 
       (ramb) 
       (ramb (car alist) (list-ramb (cdr alist))))) 

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (set! *unparsed* (cdr *unparsed*))
  (list-ramb (cdr word-list)))