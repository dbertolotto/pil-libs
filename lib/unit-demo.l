### Unit test library demo
(load "unit.l")
(symbols '(pico unit))

#(defsuite test-arithmetic)

(deftest test-+ "Test plus operation"
  (check=
    ((+ 1 2) 3)
    ((+ -1 -3) -5)
    ))

(deftest test-/ "Test divide operation"
  (check=
    (3 (/ 6 2))
    ((+ 1 1) (/ 6 2))
    ))

#(defsuite other-tests)

(deftest test-T "Test True check"
  (checkT
    (T)
    (1)
    (NIL)
    ))

(deftest test-NIL "Test NIL check"
  (checkN
    (T)
    (1)
    (NIL)
    ))

(deftest test-throw "Test throw"
  (checkThrow
    (T "error" (throw 'an "error"))
    ('an "error" (throw 'an "error"))
    ('wrongtag "error" (throw 'an "error"))
    (T "wrongerror" (throw 'a "error"))
    ))
#(-run-suites)
#(bye)

