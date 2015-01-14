#lang planet neil/sicp
(f 'x 'y)
;all the saves and restores are superfluous


((f) 'x 'y)
;all the saves and restores are superfluous


(f (g 'x) y)
;save env before f is superfluous

(f (g 'x) 'y)
;save env before eval f ,
;and save env before eval (g 'x)
;are superfluous
