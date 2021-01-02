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
      (if (str? (: D)) (prinl (: D)) (println (: D))))
    (unless (: R)
      (yellow
        (prin "Check form: ")
        (println (: F))
        (prin "  Expected: `")
        (print (: E))
        (prinl "`")
        (prin "  Actual: `")
        (print (: EF))
        (prinl "`")))))

(class +Test +TestReport)
(dm T (Expected Form Doc)
  (=: E Expected)
  (=: F Form)
  (=: D (if Doc Doc (list '= Expected Form)))
  (push '*Tests This))
(dm run> ()
  (=: EF (eval (: F)))
  (=: R (= (: E) (: EF)))
  (=: R? T))

(class +AssertEquals +Test)

(de a= (Expected Form Doc)
  (new '(+AssertEquals) Expected Form Doc))

(de aT (Form Doc)
  (new '(+AssertEquals) T Form Doc))

(de aNil (Form Doc)
  (new '(+AssertEquals) NIL Form Doc))

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
(a= 3 '(+ 1 1) "assert fail")
(aT '(not NIL))
(aT 'NIL)
(aNil '(not NIL))
(aNil 'NIL)

