(set-logic QF_UF)
(declare-sort S0 0)
(declare-sort S1 0)
(declare-fun p0 ( S0) Bool)
(declare-fun v0 () S0)
(assert 
 (let ((?n1 (p0 v0))) 
 (let ((?n2 false))
 (let ((?n3 (= ?n2 ?n1))) 
 (let ((?n4 (xor ?n2 ?n3))) 
 (let ((?n5 (= ?n1 ?n4))) ?n5
))))))
(check-sat)
(exit)
