### Unit test library

(setq *TestName NIL)

(de report-result (Res Form)
  (println (if Res 'pass 'FAIL) '... *TestName ': Form)
  Res)

(de combine-results @
  (full (rest)))

(de check Forms
  (full (mapcar '((F) (report-result (eval F) F)) Forms)))

(de detest Args
  (let (@Name (car Args)
        @Params (cadr Args)
        @Body (cddr Args))
    (macro
      (de @Name @Params
        (let *TestName (append *TestName (list '@Name))
          ~'@Body)))))

(detest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -5)))

(detest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(detest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(test-arithmetic)
