(set-logic QF_UF)
(declare-sort S1 0)
(declare-sort S0 0)
(declare-fun v0 () S0)
(assert 
 (let ((?n1 true))
 (let ((?n2 (ite ?n1 v0 v0))) 
 (let ((?n3 (= v0 ?n2))) 
 (let ((?n4 (xor ?n1 ?n3))) ?n4
)))))
(check-sat)
(exit)
