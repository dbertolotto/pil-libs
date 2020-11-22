#(symbols 'unit 'pico)

(class +Test)
(dm T ())
(dm run> ())
(dm run?> () (: R?))
(dm res> () (: R))
(dm print> ())

(class +AssertEquals +Test)
(dm T (Expected . Form)
  (=: E Expected)
  (=: F Form))
(dm run> ()
  (=: EF (eval (car (: F))))
  (=: R (= (: E) (: EF)))
  (=: R? T))

(setq A (new +AssertEquals 2 (+ 1 1)))
