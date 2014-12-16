#lang planet neil/sicp

;1
;The professor lectures to [the student in (the class with the cat)]
;教授给有猫的教室里的学生上课
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase (prep to)
               (noun-phrase
                (simple-noun-phrase
                 (article the) (noun student))
                (pre-phrase (prep in)
                            (noun-phrase
                             (simple-noun-phrase
                              (article the) (noun class))
                             (pre-phrase (prep with)
                                         (simple-noun-phrase
                                          (article the) (noun cat)))))))))
;2
;The professor lectures to [(the student in the class) with the cat]
;教授给教室里的学生和猫(不一定在教室里)上课
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase (prep to)
               (noun-phrase
                (noun-phrase
                 (simple-noun-phrase
                  (article the) (noun student))
                 (pre-phrase (prep in)
                             (simple-noun-phrase
                              (article the) (noun class))))
                (pre-phrase (prep with)
                            (simple-noun-phrase
                             (article the) (noun cat)))))))
;3
;The professor (lectures to the student) in (the class with the cat)
;教授在有猫的教室里给学生(不一定在教室)上课
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase (prep to)
                (simple-noun-phrase
                 (article the) (noun student))))
  (prep-phrase (prep in)
               (noun-phrase
                (simple-noun-phrase
                 (article the) (noun class))
                (pre-phrase (prep with)
                            (simple-noun-phrase
                             (article the) (noun cat)))))))
                             

;4
;The professor [lectures to (the student in the class)] with the cat
;教授带着猫给教室里的学生上课
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (pre-phrase (prep to)
               (noun-phrase
                (simple-noun-phrase
                 (article the) (noun student))
                (pre-phrase (prep in)
                            (simple-noun-phrase
                             (article the) (noun class))))))
  (prep-phrase (prep with)
               (simple-noun-phrase
                (article the) (noun cat)))))

;5
;The professor [(lectures to the student) in the class] with the cat
;教授带着猫在教室里给学生上课
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (pre-phrase (prep to)
                (simple-noun-phrase
                 (article the) (noun student))))
   (pre-phrase (prep in)
               (simple-noun-phrase
                (article the) (noun class))))
  (prep-phrase (prep with)
               (simple-noun-phrase
                (article the) (noun cat)))))

