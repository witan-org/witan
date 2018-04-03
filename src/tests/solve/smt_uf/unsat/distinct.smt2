(set-logic QF_UF)
(declare-sort S0 0)
(assert 
 (let ((?n1 true))
 (let ((?n2 false))
 (let ((?n3 (xor ?n1 ?n2))) 
 (let ((?n4 (xor ?n1 ?n3))) ?n4
)))))
(check-sat)
(exit)
