### Unit test library
(symbols 'unit 'pico)

(local) *TestColor *TestName *TestPassMark *TestFailMark

(setq *TestColor T)
(setq *TestName NIL)
(setq *TestPassMark (char (hex "2714")))
(setq *TestFailMark (char (hex "2718")))

(local) with-color red green yellow

(de with-color (Color . @Body)
  (when *TestColor (call "tput" "setaf" Color))
  (macro ~'@Body)
  (when *TestColor (call "tput" "sgr0")))

(de red @Body
  (macro (with-color "1" ~'@Body)))

(de green @Body
  (macro (with-color "2" ~'@Body)))

(de yellow @Body
  (macro (with-color "3" ~'@Body)))

(local) report-result check

(de report-result (Comp Forms)
  (let (FullForm (cons Comp Forms)
        Result (eval FullForm))
    (if Result
      (green (prin *TestPassMark))
      (red (prin *TestFailMark)))
    (prin " ... ")
    (print *TestName)
    (prin " : ")
    (println FullForm)
    (unless Result
      (yellow
        (prinl "Check: " Comp)
        (for (I . F) Forms
          (prin (text "   F@1: " I))
          (println (eval F)))))
    Result))

(de check (Comp . Forms)
  (let report-fn '((F) (report-result Comp F))
    (full (mapcar report-fn Forms))))

(local) check=

(de check= @Forms
  (macro (check '= ~'@Forms)))

(local) combine-results deftest

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

