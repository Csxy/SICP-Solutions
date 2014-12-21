#lang planet neil/sicp

;a)
(meeting ?department (Friday ?time))

;b)
(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?rest))
           (or (meeting ?division ?day-and-time)
               (meeting whole-company ?day-and-time))))

;c)
(meeting-time (Hacker Alyssa P) (wednesday ?time))