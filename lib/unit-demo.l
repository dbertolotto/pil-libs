### Unit test library demo
(load "unit.l")
(symbols 'pico 'unit)

(deftest test-- ()
  (check=
    (4 (- 6 2))
    ((+ 1 1) (- 6 2))
    ))

#(defsuite test-arithmetic)

(deftest test-+ ()
  (check=
    ((+ 1 2) 3)
    ((+ 1 2 3) 6)
    ((+ -1 -3) -5)
    ))

(deftest test-* ()
  (check=
    ((* 2 2) 4)
    ((* 3 5) 15)
    ))

#(defsuite other-tests)

(deftest test-/ ()
  (check=
    (3 (/ 6 2))
    ((+ 1 1) (/ 6 2))
    ))

#(-run-suites)
#(bye)

