(set-logic QF_LRA)
(declare-fun z () Real)
(assert
 (let ((?3 3))
 (let ((?n2 (+ z ?3)))
 (let ((?2 2))
 (let ((?n4 (= ?n2 ?2)))
 (let ((?n5 (= z ?n2)))
 (let ((?n7 (not ?n5)))
 (let ((?n8 (and ?n4 ?n7)))
 (let ((?n9 (not ?n8))) ?n9
)))))))))
(check-sat)
(exit)
