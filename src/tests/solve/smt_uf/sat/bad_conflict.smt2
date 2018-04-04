(set-logic QF_UF)
(declare-sort S0 0)
(declare-sort S1 0)
(declare-fun p0 ( S0) Bool)
(declare-fun v0 () S0)
(assert 
 (let ((?n1 true))
 (let ((?n2 (p0 v0))) 
 (let ((?n3 (xor ?n1 ?n2))) ?n3
))))
(check-sat)
(exit)
