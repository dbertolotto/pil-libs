### Unit test library
(symbols 'unit 'pico)

(local) *TestColor *TestName *TestPassMark *TestFailMark
(setq *TestColor T)
(setq *TestName NIL)
(setq *TestPassMark (char (hex "2714")))
(setq *TestFailMark (char (hex "2718")))

(local) color prin-color prin-red prin-green

(de color (Fun Color . @)
  (when *TestColor (call "tput" "setaf" Color))
  (pass Fun)
  (when *TestColor (call "tput" "sgr0")))

(de prin-color (Color . @)
  (pass color 'prin Color))

(de prin-red @
  (pass prin-color "1"))

(de prin-green @
  (pass prin-color "2"))

(local) report-result check combine-results deftest

(de report-result (Result Form)
  (if Result
    (prin-green *TestPassMark)
    (prin-red *TestFailMark))
  (prin " ... ")
  (print *TestName)
  (prin " : ")
  (println Form)
  Res)

(de check Forms
  (let report-fn '((F) (report-result (eval F) F))
    (full (mapcar report-fn Forms))))

(de combine-results @
  (full (rest)))

(de deftest Args
  (let (@Name (car Args)
        @Params (cadr Args)
        @Body (cddr Args))
    (macro
      (de @Name @Params
        (let *TestName (append *TestName (list '@Name))
          ~'@Body)))))

