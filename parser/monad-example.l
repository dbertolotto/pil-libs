# monad example
# (see http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html?m=1)

(de f (X)
  (cons (+ 1 X) "f called."))

(de g (X)
  (cons (* 2 X) "g called."))

(de bind (@F @G)
  (fill
    '((X)
      (let ((RG . SG) ('@G X)
            (RF . SF) ('@F RG))
        (cons RF (pack SG SF))))))

(def 'h (bind f g))

(test '(5 . "g called.f called.") (h 2))

(de unit (X)
  (cons X ""))

(def 'fu (bind f unit))
(def 'uf (bind unit f))

(test (fu 3) (f 3))
(test (uf 3) (f 3))

(de lift (Fn)
  (bind Fn unit))

(def 'lflg (bind (lift f) (lift g)))
(def 'lfg (lift (bind f g)))

(test (lflg 3) (lfg 3))

