(set-logic QF_UF)
(declare-sort S0 0)
(declare-sort S1 0)
(declare-fun p2 ( S0 S1) Bool)
(declare-fun v1 () S1)
(declare-fun v0 () S0)
(assert 
 (let ((?n1 (p2 v0 v1))) 
 (let ((?n2 (not ?n1))) 
 (let ((?n3 (= ?n1 ?n2))) ?n3
))))
(check-sat)
(exit)
