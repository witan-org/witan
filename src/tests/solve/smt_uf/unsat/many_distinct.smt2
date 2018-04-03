(set-logic QF_UF)
(declare-sort S0 0)
(declare-fun ?n1 () S0)
(declare-fun ?n2 () S0)
(declare-fun ?n3 () S0)
(declare-fun ?n4 () S0)
(declare-fun ?n5 () S0)
(declare-fun ?b1 () Bool)
(declare-fun ?b2 () Bool)
(declare-fun ?b3 () Bool)

(assert
 (ite ?b1
      (and
       (distinct ?n1 ?n2)
       (ite
        ?b2
        (and
         (distinct ?n1 ?n3)
         (distinct ?n2 ?n4)
         (ite ?b3
              (and
               (= ?n1 ?n5)
               (= ?n5 ?n2)
               )
              false
              )
         )
        false
        )
       )
      false
      )
 )

(check-sat)
(exit)
