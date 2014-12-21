#lang planet neil/sicp

(rule (father ?s ?f)
      (or (son ?f ?s)
          (and (wife ?f ?m)
               (son ?m ?s))))
(rule (grandson ?g ?s)
      (and (father ?s ?f)
           (father ?f ?g)))