#lang planet neil/sicp
(assert! (f a b))
(assert! (rule (f ?x ?y)
               (f ?y ?x)))
;input:
(f a ?x)

;output when we delay it
(f a b)
(f a b)
(f a b)
......

;if we don't use "delay",there is nothing to display