(symbols 'classutil 'pico)

(local) &con
(de &con Lst
  (eval (conc (list 'dm T Lst)
              (mapcar '((L) (list '=: L L)) Lst))))

(local) &get
(de &get Lst
  (for L Lst
    (eval (list 'dm (intern (pack (lowc (name L)) ">")) '() (list ': L)))))

(class +Test) (&con X Y) (&get X Y)
(class +RefTest) (dm T (X Y) (=: X X) (=: Y Y)) (dm x> () (: X)) (dm y> () (: Y))
(setq Test (new +Test 1 2))
(setq RefTest (new +RefTest 1 2))
(test (x> RefTest) (x> Test))
(test (y> RefTest) (y> Test))

