(load+ "#lib/coll.l")

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

