#lang planet neil/sicp

(rule (replace ?person-1 ?person-2)
      (and (job ?person-1 ?work1)
           (job ?person-2 ?work2)
           (or (same ?work1 ?work2))
           (can-do-job ?work1 ?work2)
           (not (same ?person-1 ?person-2))))
;a)
(replace ?person (Fect Cy D))

;b)
(and (replace ?person-1 ?person-2)
     (salary ?person-1 ?amount-1)
     (salary ?person-2 ?amount-2)
     (lisp-value < ?amount-1 amount-2))