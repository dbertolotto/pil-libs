(symbols 'util 'pico)

(local) nil?
(de nil? (S)
  (= S NIL))

(local) nnil?
(de nnil? (S)
  (not (nil? S)))

(local) odd
(de odd (Lst)
  (make
    (for (I . X) Lst
      (unless (= 0 (% I 2))
        (link X)))))

(local) even
(de even (Lst)
  (make
    (for (I . X) Lst
      (when (= 0 (% I 2))
        (link X)))))

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

