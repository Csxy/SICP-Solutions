#lang planet neil/sicp

(define (order-dwelling? dwellings)
  (apply
   (lambda (baker cooper fletcher miller smith) 
     (not (or
           (= baker 5)
           (= cooper 1)
           (= fletcher 5)
           (= fletcher 1)
           (not (> miller cooper))
           (= (abs (- smith fletcher)) 1)
           (= (abs (- fletcher cooper)) 1)
           (not (distinct? (list baker cooper fletcher miller smith))))))
   dwellings))

(define (flatmap proc alist)
  (if (null? alist)
      '()
      (let ((result (proc (car alist))) 
            (rest (flatmap proc (cdr alist)))) 
        (if (pair? result) 
            (append result rest) 
            (cons result rest))))) 

(define (make-sequences the-list n)
  (define (helper num)
    (if (= num 0)
        '(())
        (flatmap (lambda (x) (map (lambda (y) (cons x y))
                                  (helper (- num 1))))
                 the-list)))
  (helper n))

(define (mutiple-dwelling)
  (filter order-dwelling?
          (make-sequences '(1 2 3 4 5) 5)))
; filter form Chapter2