#lang planet neil/sicp
(assert! (f a b))
(assert! (f a b c))
(assert! (rule (f ?x ?y)
               (f ?y ?x)))
;input:
(or (f a ?x)
    (f a b ?y))

;the output when we use "interleave-delayed"
(or (f a b) (f a b ?y))
(or (f a ?x) (f a b c))
(or (f a b) (f a b ?y))
(or (f a b) (f a b ?y))
......

;the output when we use "stream-append-delayed"
(or (f a b) (f a b ?y))
(or (f a b) (f a b ?y))
(or (f a b) (f a b ?y))
(or (f a b) (f a b ?y))
......