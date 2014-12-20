#lang planet neil/sicp
;a)
(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?where))

;b)
(and (salary (Bitdiddle Ben) ?Ben-salary) 
     (salary ?person ?amount)
     (lisp-value < ?amount ?Ben-salary))

;c)
(and (supervisor ?person ?boss)
     (not (job ?boss (computer . ?type)))
     (job ?boss ?work))