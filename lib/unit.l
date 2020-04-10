### Unit test library
(local) '(detest check combine-results)

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

