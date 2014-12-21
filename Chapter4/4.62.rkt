#lang planet neil/sicp

(rule (last-pair (?x) (?x)))

(rule (last-pair (?x . ?y) (?v))
      (last-pair ?y (?v)))