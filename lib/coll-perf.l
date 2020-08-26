(load+ "#lib/coll.l")


(de make-keys-ins (N)
  (let Keys '())
    (while (< (length Keys) N)
      (push1 'Keys (rand)))
    Keys)

(de make-keys-get (M Keys)
  (let (N (length Keys)
        Idx (make (do M (link (rand 1 N)))))
    (mapcar '((X) (car (nth Keys X))) Idx)))

(de do-ins (Type Keys)
  (let Table (new (list Type))
    #(prin (text "@1 insertions in @2: " (length Keys) Type))
    #(flush)
    #(bench (for K Keys (put> Table K "val")))
    (for K Keys (put> Table K "val"))
    Table))

(de do-get (Table Keys)
  (prin (text "@1 gets in @2 of size @3: " (length Keys) (car (type Table)) (size> Table)))
  (flush)
  (bench (for X Keys (get> Table K))))


(seed (time))

# table sizes
(setq Ns (5 10 20 50 100 200 500 1000 2000 100000))

(for N Ns

  (gc 300)
  (setq InsKeys (make-keys-ins N))

  (when (<= N 2000)
    (setq A (do-ins '+AList InsKeys)))
  (setq B (do-ins '+BTree InsKeys))

  (if (<= N 2000)
    (setq M 200000)
    (setq M 1000000))

  (setq GetKeys (make-keys-get M InsKeys))

  (when (<= N 2000)
    (do-get A GetKeys))
  (do-get B GetKeys)

  )
