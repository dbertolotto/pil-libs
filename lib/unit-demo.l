### Unit test library demo
(load "unit.l")
(symbols '(pico unit))

(detest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -5)))

(detest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(detest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(test-arithmetic)

(bye)

