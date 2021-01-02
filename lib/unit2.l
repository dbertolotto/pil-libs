#(symbols 'unit 'pico)

(local) *Tests *TestName *TestColor *TestShowSuccess *TestPassMark *TestFailMark

(setq *Tests NIL)
(setq *TestName NIL)
(setq *TestColor T)
(setq *TestShowSuccess T)
(setq *TestStopOnError NIL)
(setq *TestPassMark (char (hex "2714")))
(setq *TestFailMark (char (hex "2718")))

(local) with-color red green yellow cyan

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

(de cyan @Body
  (macro (with-color "6" ~'@Body)))

(local) report-result

(de report-result (Test)
  (let (Form (get Test 'F)
        Result (get Test 'R)
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
      (println Form))
    (when Failure
      (setq *TestsFailed (inc *TestsFailed))
      (yellow
        (prin "Check form: ")
        (println Form)
        (prin "Expected: `")
        (print (: E))
        (prin "` but got: `")
        (print (: EF))
        (prinl "`"))
      (if *TestStopOnError
        (quit "Error in test; check Comp, Forms, FullForm, Result")))
    Result))

(class +TestReport)
(dm report> ()
  (let (Success (and (: R) *TestShowSuccess)
        Failure (not (: R)))
    (when Success
      (green (prin *TestPassMark)))
    (when Failure
      (red (prin *TestFailMark)))
    (when (or Success Failure)
      (prin " ... ")
      (print *TestName)
      (prin " : ")
      (println (: F)))
    (unless (: R)
      (yellow
        (prin "Check form: ")
        (println (: F))
        (prin "Expected: `")
        (print (: E))
        (prin "` but got: `")
        (print (: EF))
        (prinl "`")))))

(class +Test +TestReport)
(dm T (Expected Form)
  (=: E Expected)
  (=: F Form)
  (push '*Tests This))
(dm run> ()
  (=: EF (eval (: F)))
  (=: R (= (: E) (: EF)))
  (=: R? T))

(class +AssertEquals +Test)

(class +AssertTrue +AssertEquals)
(dm T (Form)
  (super T Form))

(class +AssertNil +AssertEquals)
(dm T (Form)
  (super NIL Form))

(de a= (Expected Form)
  (new '(+AssertEquals) Expected Form))

(de aT (Expected Form)
  (new '(+AssertTrue) Expected Form))

(de aN (Expected Form)
  (new '(+AssertNil) Expected Form))

(de run-test (Test)
  (run> Test)
  (report> Test))

(de run-tests ()
  (mapcar 'run-test *Tests)
  (summary))

(de tests-ran ()
  (length (filter '((Test) (get Test 'R?)) *Tests)))

(de tests-passed ()
  (length (filter '((Test) (get Test 'R)) *Tests)))

(de summary ()
  (let (TestsRan (tests-ran)
        TestsPassed (tests-passed))
    (prinl)
    (prin "Test summary")
    (prin " -- ")
    (green (prin "passed: " TestsPassed))
    (prin " -- ")
    (red (prin "failed: " (- TestsRan TestsPassed)))
    (prinl)))

(a= 2 '(+ 1 1))
(a= 3 '(+ 1 1))
(aT '(not NIL))
(aT 'NIL)
(aN '(not NIL))
(aN 'NIL)

