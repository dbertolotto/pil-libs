### Unit test library
#(symbols 'unit 'pico)

(local) *Tests *TestName *TestColor *TestShowSuccess *TestPassMark *TestFailMark

(setq *Tests NIL)
(setq *TestName NIL)
(setq *TestColor T)
(setq *TestShowSuccess T)
(setq *TestStopOnError NIL)
(setq *TestPassMark (char (hex "2714")))
(setq *TestFailMark (char (hex "2718")))

(setq *TestsRan 0)
(setq *TestsFailed 0)

#(setq *TestSuites NIL)
#(setq *TestCurrSuite NIL)

#(de add-test (Name)
#  (let (Registered (cdr (assoc *TestCurrSuite *Tests))
#        New (append Registered (list Name)))
#    (push '*Tests (cons *TestCurrSuite New))))

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
        Result (eval FullForm)
        Success (and Result *TestShowSuccess)
        Failure (not Result))
    (setq *TestsRan (inc *TestsRan))
    (when Success
      (green (prin *TestPassMark)))
    (when Failure
      (red (prin *TestFailMark)))
    (when (or Success Failure)
      (prin " ... ")
      (print *TestName)
      (prin " : ")
      (println FullForm))
    (when Failure
      (setq *TestsFailed (inc *TestsFailed))
      (yellow
        (prinl "Check: " Comp)
        (for (I . F) Forms
          (prin (align 8 (text "F@1: " I)))
          (println (eval F))))
      (if *TestStopOnError
        (quit "Error in test; check Comp, Forms, FullForm, Result")))
    Result))

(de check (Comp . Forms)
  (let report-fn '((F) (report-result Comp F))
    (full (mapcar report-fn Forms))))

(local) check= checkN checkT

(de check= @Forms
  (macro (check '= ~'@Forms)))

(de checkN @Forms
  (macro (check 'not ~'@Forms)))

(de checkT @Forms
  (macro (check '=T ~'@Forms)))

(local) combine-results defsuite deftest

(de combine-results @
  (full (rest)))

#(de defsuite Args
#  (let (Name (car Args))    # Suite name
#    (push '*TestSuites Name)
#    (setq *TestCurrSuite Name)))

(de deftest Args
  (let (@Name (car Args)    # Test name
        @Params (cadr Args) # Test args
        @Body (cddr Args))  # Test body
    (macro
      (de @Name @Params
        (let *TestName (append *TestName (list '@Name))
          ~'@Body)))
    (add-test @Name)))

(de add-test (Name)
  (push '*Tests Name))

(local) -run-suites -run-tests -no-color -no-success

#(de -run-suites @
#  (let Suites (if (args) (rest) (flip *TestSuites))
#    (for Suite Suites
#      (prinl "\nRunning Suite: " Suite)
#      (apply '-run-tests (cdr (assoc Suite *Tests))))))

(de -run-tests @
  (prinl)
  (let Tests (if (args) (rest) (flip *Tests))
    (for Test Tests
      (Test)))
  (summary))

(de summary ()
  (prinl)
  (prin "Test summary")
  (prin " -- ran: " *TestsRan)
  (prin " -- failed: " *TestsFailed)
  (prinl))

(de -no-color ()
  (prinl "* Colors off")
  (setq *TestColor NIL))

(de -no-success ()
  (prinl "* Showing only failed tests")
  (setq *TestShowSuccess NIL))

(de -stop-on-error ()
  (prinl "* Stop on error")
  (setq *TestStopOnError T))

# default suite
#(defsuite no-suite)
