#lang planet neil/sicp

(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?type))
           (or (not (supervisor ?person ?boss))
               (and (supervisor ?person ?superiors)
                    (not (job ?superiors (?division . ?type2)))))))