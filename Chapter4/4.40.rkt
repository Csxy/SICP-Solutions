#lang planet neil/sicp

; 5^5 kinds

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 3 4 5)))
    (require (not (equal? cooper baker))))
    (let ((fletcher (amb 2 3 4)))
      (require (not (member fletcher (list baker cooper))))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((miller (amb 3 4 5)))
        (require (not (member miller (list baker cooper fletcher))))
        (require (> miller cooper))
        (let ((smith (amb 1 2 3 4 5)))
          (require (not (member smith (list baker cooper fletcher miller))))
          (require (not (= (abs (- smith fletcher)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith))))))
                
