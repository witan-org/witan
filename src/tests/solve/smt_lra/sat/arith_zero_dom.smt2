(set-logic QF_LRA)
(declare-fun v2 () Real)
(declare-fun v1 () Real)
(declare-fun b () Bool)
(assert
 (let ((?1 1) (?0 0))
 (let ((?ite (ite b ?0 ?1)))
 (let ((?n7 (= ?1 ?ite))) ?n7
))))
(check-sat)
(exit)
