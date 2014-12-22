#lang planet neil/sicp

(rule ((great . ?rel) ?x ?y)
      (and (father ?x ?middle)
           (?rel ?middle ?y)))