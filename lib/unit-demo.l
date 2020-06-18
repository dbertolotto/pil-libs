### Unit test library demo
(load "unit.l")
(symbols 'pico 'unit)

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

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-/ ()
  (check=
    (3 (/ 6 2))
    ((+ 1 1) (/ 6 2))
    ))

(test-arithmetic)
(test-/)

(bye)

