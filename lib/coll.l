(class +AList)
(dm put> (K V)
  (del> This K)
  (push (:: D) (cons K V)))
(dm del> (K)
  (if (assoc K (: D))
    (del @ (:: D))))
(dm get> (K)
  (cdr (assoc K (: D))))

(class +BTree)
(dm put> (K V)
  (del> This K)
  (idx (:: D) (cons K V) T))
(dm del> (K)
  (if (lup (: D) K)
    (idx (:: D) @ NIL)))
(dm get> (K)
  (cdr (lup (: D) K)))
(dm bal> ()
  (balance (:: D) (idx (:: D))))

(setq Data '(
  ("xkcd" . 24)
  (pino . "aaa")
  ("Bro" . kudos)
  (?!= . "help")
  (12 . "ecce")
  (home . "gnappo")
  ("cave" . "canem")
  ("mile" . 250)
  (3 . 400)
  ("53" . `(char (hex "6090")))
  (auth . cred)
  (€$ . 44)))

(setq A (new '(+AList)))
(for X Data (let ((K . V) X) (put> A K V)))

(setq B (new '(+BTree)))
(for X Data (let ((K . V) X) (put> B K V)))

(seed (time))

(setq Ns (5 10 20 50 100 200 500 1000 2000 5000 10000))
(for N Ns

(setq A (new '(+AList)))
(setq B (new '(+BTree)))
(gc 100)

#(setq N 100)
(setq InsKeys '())
(while (< (length InsKeys) N)
  (push1 'InsKeys (rand)))

(prin (text "@1 insertions in +AList: " N)) (flush)
(bench (for K InsKeys (put> A K "val")))
(for K InsKeys (put> A K "val"))

(prin (text "@1 insertions in +BTree: " N)) (flush)
(bench (for K InsKeys (put> B K "val")))
(for K InsKeys (put> B K "val"))

(setq M 200000)
(setq Idx (make (do M (link (rand 1 N)))))
(setq GetKeys (mapcar '((X) (car (nth InsKeys X))) Idx))

(prin (text "@1 gets in +AList of size @2: " M (align 5 N))) (flush)
(bench (for X GetKeys (get> A K)))

(prin (text "@1 gets in +BTree of size @2: " M (align 5 N))) (flush)
(bench (for X GetKeys (get> B K)))

#(prin (text "@1 gets in bal +BTree of size @2: " M (align 5 N))) (flush)
#(bal> B)
#(bench (for X GetKeys (get> B K)))

)

