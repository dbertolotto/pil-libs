# combinatory parser
(symbols 'parser 'pico)

(setq *ParserError NIL)

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

(de label (Label Obj)
  (when Label
    (label> Obj Label))
  Obj)

(class +Parser +PLabel)
(dm T (Label Fun)
  (=: Label Label)
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
    #(prinl R " " C " " (: Inp))
    (new '(+PInp) (cdr (: Inp)) (new '(+PPos) R C))))

(de err-parse (Inp X)
  (new '(+PErr) (text "Unexpected '@1'" X) (pos> Inp)))

(de err-no-inp (Inp)
  (new '(+PErr) "No more Input" (pos> Inp)))

(de add-err (R)
  (setq *ParserError (insert 0 *ParserError (m> R)))
  R)

(de showerr ()
  (mapc 'prinl (head 5 *ParserError)))

(de defp Args
  (let (@Name (car Args)
        @Params (cadr Args)
        @Label (caddr Args)
        @Body (cdddr Args))
    (macro
      (de @Name @Params
        (new '(+Parser) @Label
          (fill '((Inp)
            ~'@Body)))))))

(de satisfy (Pred Inp)
  (let X (car> Inp)
    (cond
      ((= X NIL) (add-err (err-no-inp Inp)))
      (('Pred X) (new '(+PSuc) X (cdr> Inp)))
      (T  (add-err (err-parse Inp X))))))

(defp charp (@C)
  (text "char '@1'" @C)
  (satisfy (fill '((X) (= X @C))) Inp))

(defp digit19p ()
  "digit[1-9]"
  (satisfy '((X) (member X *Digit19)) Inp))

(defp digitp ()
  "digit"
  (satisfy '((X) (member X *Digit)) Inp))

(defp wsp ()
  "whitespace"
   (satisfy '((X) (member X *Ws)) Inp))

(defp lowcp ()
  "lowercase"
  (satisfy '((X) (member X *Lowcase)) Inp))

(defp uppcp ()
  "uppercase"
  (satisfy '((X) (member X *Uppcase)) Inp))

(defp alphap ()
  "alphanumeric"
  (satisfy '((X) (member X *Alpha)) Inp))

(defp hexp ()
  "hexadecimal"
    (satisfy '((X) (member X *Hex)) Inp))

(de to-parser (P)
  (if (isa '(+Parser) P)
    P
    (eval P)))

(de doparse (P Inp)
  (parse> (to-parser P) Inp))

(defp andp (@P1 @P2)
  (text "(@1 AND @2)" (get (to-parser @P1) 'Label) (get (to-parser @P2) 'Label))
  (let R1 (doparse '@P1 Inp)
    (if (isa '+PSuc R1)
      (let R2 (doparse '@P2 (r> R1))
        (if (isa '+PSuc R2)
          (new '(+PSuc) (list (v> R1) (v> R2)) (r> R2))
          (add-err R2)))
      (add-err R1))))

(defp fp (@Fn @P)
  "unknown"
  (let R (doparse '@P Inp)
    (if (isa '+PSuc R)
      (new '(+PSuc) ('@Fn (v> R)) (r> R))
      (add-err R))))

(de mapp (@Fn P)
  (fp (fill '((X) (mapcar '@Fn X))) P))

(de applyp (@Fn P)
  (fp (fill '((X) (apply '@Fn X))) P))

(defp returnp (X)
  (text "return @1" X)
  (new '(+PSuc) X Inp))

(de sequencep @PLst
  (if @PLst
    (new '(+Parser) "sequence"
      (fill
        '((Inp)
          (let R1 (doparse (car '@PLst) Inp)
            (if (isa '(+PSuc) R1)
              (let R2 (doparse (eval (cons 'sequencep (cdr '@PLst))) (r> R1))
                (if (isa '(+PSuc) R2)
                  (new '(+PSuc) (cons (v> R1) (v> R2) ) (r> R2))
                  (add-err R2)))
              (add-err R1))))))
    (returnp NIL)))

(defp orp @PLst
  (text "any of (@1)" (pack (maplist '((L) (pack (car L) (when (cdr L) ",")))
                                     (mapcar '((P) (get (to-parser P) 'Label))
                                             @PLst))))
  (let (R1 (doparse (car '@PLst) Inp)
        Cdr (cdr '@PLst))
    (if (and (not (isa '(+PSuc) R1)) Cdr)
      (doparse (eval (cons 'orp Cdr)) Inp)
      R1)))

(de stringp (Str)
  (label (text "string '@1'" Str)
    (fp 'pack
      (eval (cons 'sequencep (mapcar 'charp (chop Str)))))))

(defp manyp (@P)
  (text "many @1" (get (to-parser @P) 'Label))
  (let R1 (doparse '@P Inp)
    (if (isa '+PSuc R1)
      (let R2 (doparse (manyp '@P) (r> R1))
        (if (isa '(+PSuc) R2)
          (new '(+PSuc) (cons (v> R1) (v> R2)) (r> R2))
          (prog
            (add-err R2)
            R1)))
      (new '(+PSuc) '() Inp))))

(de many1p (P)
  (label (text "many1 @1" (get (to-parser P) 'Label))
    (fp '((X) (cons (car X) (cadr X)))
        (andp P (manyp P)))))

(defp optp (@P)
  (text "opt @1" (get (to-parser @P) 'Label))
    (let R (doparse '@P Inp)
      (if (isa '+PSuc R)
        R
        (new '(+PSuc) '() Inp))))

(defp droprp (@P1 @P2)
  (text "drop @1" (get (to-parser @P2) 'Label))
  (let R1 (doparse '@P1 Inp)
    (if (isa '+PSuc R1)
      (let R2 (doparse '@P2 (r> R1))
        (if (isa '+PSuc R2)
          (new '(+PSuc) (v> R1) (r> R2))
          (add-err R2)))
      (add-err R1))))

(defp droplp (@P1 @P2)
  (text "drop @1" (get (to-parser @P1) 'Label))
  (let R1 (doparse '@P1 Inp)
    (if (isa '+PSuc R1)
      (let R2 (doparse '@P2 (r> R1))
        (if (isa '+PSuc R2)
          (new '(+PSuc) (v> R2) (r> R2))
          (add-err R2)))
      (add-err R1))))

(de betweenp (P1 P2 P3)
  (label (text "between @1 and @2" (get (to-parser P1) 'Label) (get (to-parser P3) 'Label))
    (droprp (droplp P1 P2) P3)))

(defp sepbyp (@P @Psep)
  (text "sepby @1" (get (to-parser @Psep) 'Label))
  (let R1 (doparse '@P Inp)
    (if (isa '+PSuc R1)
      (let R2 (doparse (manyp (droplp '@Psep '@P)) (r> R1))
        (if (isa '(+PSuc) R2)
          (new '(+PSuc) (cons (v> R1) (v> R2)) (r> R2))
          (prog
            (add-err R2)
            R1)))
      (new '(+PSuc) '() Inp))))

(defp sepby1p (@P @Psep)
  (text "sepby1 @1" (get (to-parser @Psep) 'Label))
  (let R1 (doparse '@P Inp)
    (if (isa '+PSuc R1)
      (let R2 (doparse (manyp (droplp '@Psep '@P)) (r> R1))
        (new '(+PSuc) (cons (v> R1) (v> R2)) (r> R2)))
      (add-err R1))))

(def 'intp
  (fp 'list
    (fp 'format
      (andp (optp (orp (charp "+") (charp "-")))
            (many1p (digitp))))))

(defp ignorep (@P @R)
  "ignore"
  (let R (doparse '@P Inp)
    (if (isa '+PSuc R)
      (new '(+PSuc) @R (r> R))
      R)))

