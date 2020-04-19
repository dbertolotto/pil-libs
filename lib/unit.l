### Unit test library
(local) '(detest check combine-results)

(setq *ColorRes T)
(setq *TestName NIL)

(de color (Fun Color . @)
  (when *ColorRes (call "tput" "setaf" Color))
  (pass Fun)
  (when *ColorRes (call "tput" "sgr0")))

(de prin-color (Color . @)
  (pass color 'prin Color))

(de prin-green @
  (pass prin-color "2"))

(de prin-red @
  (pass prin-color "1"))

(de print-color (Color . @)
  (pass color 'print Color))

(de print-yellow @
  (pass print-color "3"))

(de report-result (Res Form)
  (if Res (prin-green "pass") (prin-red "FAIL"))
  (prin " ... ")
  (print-yellow *TestName)
  (prin " : ")
  (println Form)
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

