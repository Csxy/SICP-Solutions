#lang planet neil/sicp

(controller
 sqrt-loop
   (assign x (op read))
   (assign guess (const 1.0))
 test-g
   (assign t (op square) (reg guess))
   (assign t (op -) (reg t) (reg x))
   (assign t (op abs) (reg t))
   (test (op <) (reg t) (const 0.001))
   (branch (label sqrt-done))
 improve
   (assign t (op /) (reg x) (reg guess))
   (assign guess (op average) (reg guess) (reg t))
   (goto (label test-g))
 sqrt-done
   (perform (op print) (reg guess))
   (goto (label sqrt-loop)))