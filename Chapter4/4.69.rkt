#lang planet neil/sicp

(rule ((grandson) ?x ?y) 
       (grandson ?x ?y)) 
(rule ((great . ?rel) ?x ?y)
      (and (father ?middle ?x)
           (?rel ?middle ?y)))