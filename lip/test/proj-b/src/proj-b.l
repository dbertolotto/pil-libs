(symbols 'proj-b 'proj-c 'pico)

(prinl "Load proj-b")

(setq *Var 2)

(de f (X)
  (+ *Var proj-c~*Var 1))

(prinl (f 1))
