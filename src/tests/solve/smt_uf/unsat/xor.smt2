(set-logic QF_UF)
(declare-fun _substvar_1662_ () Bool)
(declare-fun _substvar_2244_ () Bool)
(assert
        (let ((e183 (xor _substvar_2244_ _substvar_2244_)))
         (let ((e184 (not e183)))
          (let ((e185 (= _substvar_1662_ _substvar_1662_)))
           (let ((e186 (xor e184 e185)))
            e186)))))
(check-sat)

