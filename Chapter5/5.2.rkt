#lang planet neil/sicp

(controller
 (assign product (const 1))
 (assign counter (const 1))
 test-p
   (test (op >) (reg counter) (reg n))
   (branch (label iter-done))
   (assign product (op *) (reg product) (reg counter))
   (assign product (op +) (reg counter) (const 1))
   (goto (label test-p))
 iter-done)