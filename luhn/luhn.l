(de drop (Lst N)
  (make
    (for (I . X) Lst
      (unless (=0 (% I N))
        (link X)))))

(de take (Lst N)
  (make
    (for (I . X) Lst
      (if (=0 (% I N))
        (link X)))))

(de prep-num (N)
  (reverse (chop N)))

(de odd (Lst)
  (mapcar 'format (drop Lst 2)))

(de even (Lst)
  (mapcar 'format (take Lst 2)))

(de sum-odd (Lst)
  (apply + (odd Lst)))

(de double-even (Lst)
  (mapcar '((X)(* 2 X)) (even Lst)))

(de sum-double-even (Lst)
  (apply + (mapcar '((X)(if (> X 9) (- X 9) X)) (double-even Lst))))

(de total-sum (Lst)
  (+ (sum-odd Lst) (sum-double-even Lst)))

(de luhn-sum (N)
  (total-sum (prep-num N)))

(de luhn-valid? (N)
  (=0 (% (luhn-sum N) 10)))

(de test-luhn ()
  (test 0 (luhn-valid? 49927398716))
  (test NIL (luhn-valid? 49927398717))
  (test NIL (luhn-valid? 1234567812345678))
  (test 0 (luhn-valid? 1234567812345670)))
