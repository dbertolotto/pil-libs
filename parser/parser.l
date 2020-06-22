# combinatory parser
(symbols 'parser 'pico)

(setq *Lowcase (make (for I 26 (link (char (+ 96 I))))))
(setq *Uppcase (mapcar uppc *Lowcase))
(setq *Digit (mapcar format (range 0 9)))
(setq *Digit19 (mapcar format (range 1 9)))
(setq *Ws (chop " \t\n\r"))
(setq *Alpha (conc *Lowcase *Uppcase *Digit))
(setq *Hex (conc (chop "ABCDEFabcdef") *Digit))

(class +PLabel)
(dm label> (NewLabel)
  (=: Label NewLabel))
#  (when NewLabel
#    (let L (: Label)
#      (if L
#        (=: Label (pack NewLabel " => " L))
#        (=: Label NewLabel)))))

(de label (Label Obj)
  (when Label
    (label> Obj Label))
  Obj)

(class +Parser +PLabel)
(dm T (Fun)
  (=: Fun Fun))
(dm parse> (Inp)
  (label (: Label) ((: Fun) Inp)))

(class +PSuc +PLabel)
(dm T (V RInp)
  (=: V V)
  (=: RInp RInp))
(dm v> () (: V))
(dm r> () (: RInp))

(class +PErr +PLabel)
(dm T (Err Pos)
  (=: Err Err)
  (=: Pos Pos))
(dm m> ()
  (text "[r:@1,c:@2] Error while parsing @3. @4"
    (r> (: Pos)) (c> (: Pos)) (: Label) (: Err)))

(class +PPos)
(dm T (Row Col)
  (=: Row Row)
  (=: Col Col))
(dm r> () (: Row))
(dm r+> () (inc (: Row)))
(dm c> () (: Col))
(dm c+> () (inc (: Col)))

(de pos0 ()
  (new '(+PPos) 1 1))

(class +PInp)
(dm T (Inp Pos)
  (=: Inp Inp)
  (=: Pos Pos))
(dm pos> () (: Pos))
(dm car> () (car (: Inp)))
(dm cdr> ()
  (let (Next (cadr (: Inp))
        R (if (= Next "\n")
            (r+> (: Pos))
            (r> (: Pos)))
        C (cond
            ((= Next "\n") 0)
            ((= Next NIL) (c> (: Pos)))
            (T (c+> (: Pos)))))
    #(prinl R " " C " " (cdr (: Inp)))
    (new '(+PInp) (cdr (: Inp)) (new '(+PPos) R C))))

(de err-parse (Inp X)
  (new '(+PErr) (text "Unexpected '@1'" X) (pos> Inp)))

(de err-no-inp (Inp)
  (new '(+PErr) "No more Input" (pos> Inp)))

(de satisfy (@Pred)
  (new '(+Parser)
    (fill
      '((Inp)
        (let X (car> Inp)
          (cond
            ((= X NIL) (err-no-inp Inp))
            (('@Pred X) (new '(+PSuc) X (cdr> Inp)))
            (T (err-parse Inp X))))))))

(de charp (@C)
  (label "char"
    (satisfy (fill '((X) (= X @C))))))

(def 'digit0p
  (label "digit"
    (charp "0")))

(def 'digit19p
  (label "digit"
    (satisfy '((X) (member X *Digit19)))))

(def 'digitp
  (label "digit"
    (satisfy '((X) (member X *Digit)))))

(def 'wsp
  (label "whitespace"
    (satisfy '((X) (member X *Ws)))))

(def 'lowcp
  (label "lowercase"
    (satisfy '((X) (member X *Lowcase)))))

(def 'uppcp
  (label "uppercase"
    (satisfy '((X) (member X *Uppcase)))))

(def 'alphap
  (label "alphanumeric"
    (satisfy '((X) (member X *Alpha)))))

(def 'hexp
  (label "hexadecimal"
    (satisfy '((X) (member X *Hex)))))

(de to-parser (P)
  (if (isa '(+Parser) P)
    P
    (eval P)))

(de doparse (P Inp)
  (parse> (to-parser P) Inp))

(de andp (@P1 @P2)
  (new '(+Parser)
    (fill
      '((Inp)
        (let R1 (doparse '@P1 Inp)
          (if (isa '+PSuc R1)
            (let R2 (doparse '@P2 (r> R1))
              (if (isa '+PSuc R2)
                (new '(+PSuc) (list (v> R1) (v> R2)) (r> R2))
                R2))
            R1))))))

(de orp (@P1 @P2)
  (new '(+Parser)
    (fill
      '((Inp)
        (let R1 (doparse '@P1 Inp)
          (if (isa '+PSuc R1)
            R1
            (doparse '@P2 Inp)))))))

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(de fp (@Fn @P)
  (new '(+Parser)
    (fill
      '((Inp)
        (let R (doparse '@P Inp)
          (if (isa '+PSuc R)
            (new '(+PSuc) ('@Fn (v> R)) (r> R))
            R))))))

(de mapp (@Fn P)
  (fp (fill '((X) (mapcar '@Fn X))) P))

(de applyp (@Fn P)
  (fp (fill '((X) (apply '@Fn X))) P))

(de returnp (X)
  (new '(+Parser)
    (fill
      '((Inp)
        (new '(+PSuc) X Inp)))))

(de sequencep @PLst
  (if @PLst
    (new '(+Parser)
      (fill
        '((Inp)
          (let R1 (doparse (car '@PLst) Inp)
            (if (isa '(+PSuc) R1)
              (let R2 (doparse (eval (cons 'sequencep (cdr '@PLst))) (r> R1))
                (if (isa '(+PSuc) R2)
                  (new '(+PSuc) (cons (v> R1) (v> R2) ) (r> R2))
                  R2))
              R1)))))
    (returnp NIL)))

(de choicep @
  (reduce 'orp (rest)))

(de stringp (Str)
  (label (text "string '@1'" Str)
    (fp 'pack
      (eval (cons 'sequencep (mapcar 'charp (chop Str)))))))

(de manyp (@P)
  (new '(+Parser)
  (fill
    '((Inp)
       (let R1 (doparse '@P Inp)
         (if (isa '+PSuc R1)
           (let R2 (doparse (manyp '@P) (r> R1))
             (if (isa '(+PSuc) R2)
               (new '(+PSuc) (cons (v> R1) (v> R2)) (r> R2))
               R1))
           (new '(+PSuc) '() Inp)))))))

(de many1p (P)
  (fp '((X) (cons (car X) (cadr X)))
      (andp P (manyp P))))

(de optp (@P)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R (doparse '@P Inp)
        (if (isa '+PSuc R)
          R
          (new '(+PSuc) '() Inp)))))))

(de droprp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (doparse '@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 (doparse '@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (v> R1) (r> R2))
              R2))
          R1))))))

(de droplp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (doparse '@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 (doparse '@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (v> R2) (r> R2))
              R2))
          R1))))))

(de betweenp (P1 P2 P3)
  (droprp (droplp P1 P2) P3))

(de sepbyp (@P @Psep)
  (new '(+Parser)
    (fill
      '((Inp)
         (let R1 (doparse '@P Inp)
           (if (isa '+PSuc R1)
             (let R2 (doparse (manyp (droplp '@Psep '@P)) (r> R1))
               (if (isa '(+PSuc) R2)
                 (new '(+PSuc) (cons (v> R1) (v> R2)) (r> R2))
                 R1))
             (new '(+PSuc) '() Inp)))))))

(de sepby1p (@P @Psep)
  (new '(+Parser)
    (fill
      '((Inp)
         (let R1 (doparse '@P Inp)
           (if (isa '+PSuc R1)
             (let R2 (doparse (manyp (droplp '@Psep '@P)) (r> R1))
               (new '(+PSuc) (cons (v> R1) (v> R2)) (r> R2)))
             R1))))))

(def 'intp
  (fp 'list
    (fp 'format
      (andp (optp (orp (charp "+") (charp "-")))
            (many1p digitp)))))

(de ignorep (@P @R)
  (new '(+Parser)
    (fill
      '((Inp)
        (let R (doparse '@P Inp)
          (if (isa '+PSuc R)
            (new '(+PSuc) @R (r> R))
            R))))))

