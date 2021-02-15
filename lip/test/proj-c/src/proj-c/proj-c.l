(symbols 'proj-c 'pico)

(prinl "Load proj-c")

(setq *Var 1)

(de f (X)
  (+ *Var 1))

(de main ()
  (prinl "execute something: " (f 1)))

(prinl "proj-c loaded")
