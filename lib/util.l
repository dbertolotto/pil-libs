(symbols 'util 'pico)

(local) nil?
(de nil? (S)
  (= S NIL))

(local) nnil?
(de nnil? (S)
  (<> S NIL))

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

(local) reduce
(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(local) ->-
(de ->- (@X . Forms)
  (let (@Car (car Forms)
        @Cdr (cdr Forms))
    (if @Car
      (macro (->- (eval (cons (car '@Car) @X (cdr '@Car))) ~'@Cdr))
      @X)))

(local) -->
(de --> (@X . Forms)
  (let (@Car (car Forms)
        @Cdr (cdr Forms))
    (if @Car
      (macro (--> (eval (cons '@Car (list @X))) ~'@Cdr))
      @X)))
