#lang planet neil/sicp
(rule (reverse () ()))
(rule (reverse (?x . ?y) ?z)
      (and (reverse ?y ?u)
           (append-to-form ?u (?x) ?z)))