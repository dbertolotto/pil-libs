# combinatory parser
(symbols 'parser 'pico)

(setq *Lowcase (make (for I 26 (link (char (+ 96 I))))))
(setq *Uppcase (mapcar uppc *Lowcase))
(setq *Digit (mapcar format (range 0 9)))
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

(de andp-comb (V1 V2)
  (append (if (atom V1) (list V1) V1)
          (if V2 (list V2) V2)))

(de to-parser (P)
  (if (isa '(+Parser) P)
    P
    (eval P)))

(de andp (@P1 @P2)
  (new '(+Parser)
    (fill
      '((Inp)
        (let R1 (parse> (to-parser '@P1) Inp)
          (if (isa '+PSuc R1)
            (let R2 (parse> (to-parser '@P2) (r> R1))
              (if (isa '+PSuc R2)
                (new '(+PSuc) (andp-comb (v> R1) (v> R2)) (r> R2))
                R2))
            R1))))))

(de orp (@P1 @P2)
  (new '(+Parser)
    (fill
      '((Inp)
        (let R1 (parse> (to-parser '@P1) Inp)
          (if (isa '+PSuc R1)
            R1
            (parse> (to-parser '@P2) Inp)))))))

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
        (let R (parse> (to-parser '@P) Inp)
          (if (isa '+PSuc R)
            (new '(+PSuc) ('@Fn (v> R)) (r> R))
            R))))))

(de mapp (@Fn P)
  (fp (fill '((X) (mapcar '@Fn X))) P))

(de applyp (@Fn P)
  (fp (fill '((X) (apply '@Fn X))) P))

(de sequencep @
  (reduce 'andp (rest)))

(de choicep @
  (reduce 'orp (rest)))

(de stringp (Str)
  (label (text "string '@1'" Str)
    (fp 'pack
      (apply 'sequencep (mapcar 'charp (chop Str))))))

(de manyp-comb (V1 V2)
  (insert 0 V2 V1))

(de manyp (@P)
  (new '(+Parser)
  (fill
    '((Inp)
       (let R1 (parse> (to-parser '@P) Inp)
         (if (isa '+PSuc R1)
           (let R2 (parse> (manyp '@P) (r> R1))
             (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
           (new '(+PSuc) '() Inp)))))))

(de many1p (P)
  (fp '((X) (cons (car X) (cadr X)))
      (andp P (manyp P))))

(de optp (@P)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R (parse> (to-parser '@P) Inp)
        (if (isa '+PSuc R)
          R
          (new '(+PSuc) '() Inp)))))))

(de droprp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (parse> (to-parser '@P1) Inp)
        (if (isa '+PSuc R1)
          (let R2 (parse> (to-parser '@P2) (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (v> R1) (r> R2))
              R2))
          R1))))))

(de droplp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (parse> (to-parser '@P1) Inp)
        (if (isa '+PSuc R1)
          (let R2 (parse> (to-parser '@P2) (r> R1))
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
         (let R1 (parse> (to-parser '@P) Inp)
           (if (isa '+PSuc R1)
             (let R2 (parse> (manyp (droplp '@Psep '@P)) (r> R1))
               (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
             (new '(+PSuc) '() Inp)))))))

(de sepby1p (@P @Psep)
  (new '(+Parser)
    (fill
      '((Inp)
         (let R1 (parse> (to-parser '@P) Inp)
           (if (isa '+PSuc R1)
             (let R2 (parse> (manyp (droplp '@Psep '@P)) (r> R1))
               (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
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
        (let R (parse> (to-parser '@P) Inp)
          (if (isa '+PSuc R)
            (new '(+PSuc) @R (r> R))
            R))))))

### json

(def 'jnull
  (label "jnull"
    (fp 'any (stringp "null"))))

(def 'jtrue
  (label "jtrue"
    (fp 'any (stringp "true"))))

(def 'jfalse
  (label "jfalse"
    (fp 'any (stringp "false"))))

(def 'jbool
  (label "jbool"
    (orp jtrue jfalse)))

(def 'junescaped
  (satisfy '((X) (not (or (= X "\\") (= X "\""))))))

(setq *Jesc
  (list
    '("\\\"" . "\"")
    '("\\\\" . "\\")
    '("\\/" . "/")
    '("\\b" . `(char (hex "232B")))
    '("\\f" . `(char (hex "240A")))
    '("\\n" . "\n")
    '("\\r" . "\r")
    '("\\t" . "\t")
    ))

(def 'jescaped
  (apply 'choicep
         (mapcar '((X) (ignorep (stringp (car X)) (cdr X))) *Jesc)))

(def 'junicode
  (let (bl (charp "\\")
        uc (charp "u")
        hx (sequencep hexp hexp hexp hexp)
        code (fp 'char (fp 'hex (fp 'pack hx))))
    (label "junicode"
      (droplp bl (droplp uc code)))))

(def 'jchar
  (label "jchar"
    (choicep junescaped jescaped junicode)))

(def 'jstring
  (let (quotep (charp "\"")
        manych (fp 'pack (manyp jchar)))
    (label "jstring"
      (droprp (droplp quotep manych) quotep))))

(de to-num (s int fr se ex)
  (let (exn (format ex)
        exval (if exn (apply * (need exn 10)) 1)
        exlen (if (= se "-") exn 0)
        frlen (length fr))
    (setq *Scl (max *Scl (+ frlen exlen)))
    (let (num (format (pack s int "." fr) *Scl)
          op (if (= se "-") '/ '*))
      (op num exval))))

(def 'jnumber
  (let (sign (optp (orp (charp "+") (charp "-")))
        point (charp ".")
        exp (orp (charp "e") (charp "E"))
        intpart (andp sign (many1p digitp))
        fracpart (droplp point (many1p digitp))
        exppart (droplp exp intpart)
        tuple (sequencep intpart
                         (optp fracpart)
                         (optp exppart)))
    (label "jnumber"
      (applyp 'to-num (mapp 'pack tuple)))))

(def 'spacesp (manyp wsp))

(de to-array (Lst)
  (append '(T) Lst))

(def 'jarray
  (let (left (droprp (charp "[") 'spacesp)
        right (droprp (charp "]") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        value (droprp 'jvalue 'spacesp)
        values (sepbyp value comma))
    (label "jarray"
      (fp 'to-array (betweenp left values right)))))

(def 'jobject
  (let (left (droprp (charp "{") 'spacesp)
        right (droprp (charp "}") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        colon (droprp (charp ":") 'spacesp)
        key (droprp 'jstring 'spacesp)
        value (droprp 'jvalue 'spacesp)
        keyval (andp (droprp key colon) value)
        keyvals (sepbyp keyval comma))
    (label "jobject"
      (betweenp left keyvals right))))

(def 'jvalue
  (label "jvalue"
    (choicep
      'jobject
      'jarray
      'jstring
      'jnumber
      'jbool
      'jnull)))

### print

(de to-chars (File)
  (chop (in File (till NIL T))))

(de to-pinp (File)
  (new '(+PInp) (to-chars File) (pos0)))

(de read-json (Source)
  (cond
    ((isa '+PInp Source) (parse> jvalue Source))
    ((info Source) (parse> jvalue (to-pinp Source)))
    ((str? Source) (parse> jvalue (new '(+PInp) (chop Source) (pos0))))
    ((lst? Source) (parse> jvalue (new '(+PInp) Source (pos0))))
    (T (throw 'JsonParseError "cannot read source"))))

(setq *Json-pretty-print NIL)
(setq *Json-indent-char " ")
(setq *Json-indent 2)

(de print-json (Jval Indent)
  (let (Newline (when *Json-pretty-print "\n")
        Ind (if (and *Json-pretty-print Indent) Indent 0)
        Ind++ (inc Ind)
        IndStr (when *Json-pretty-print (need (* Ind *Json-indent) *Json-indent-char))
        IndStr++ (when *Json-pretty-print (need (* Ind++ *Json-indent) *Json-indent-char)))
    (cond
      ((atom Jval) # single value
        (prin IndStr)
        (cond
          ((str? Jval) (print Jval))
          ((num? Jval) (prin (format Jval *Scl)))
          ((= NIL Jval) (prin "{}"))
          (T (prin Jval))))
      ((= T (car Jval)) # array
        (prin "[" Newline)
        (map '((X)
               (let V (car X)
                 (unless (atom V) (prin IndStr++))
                 (print-json V Ind++))
               (prin (when (cdr X) ",") Newline))
             (cdr Jval))
        (prin IndStr "]"))
      (T # object
        (prin "{" Newline)
        (map '((X)
               (print-json (caar X) Ind++)
               (if *Json-pretty-print
                 (prin " : ")
                 (prin ":"))
               (let V (cadar X)
                 (if (atom V)
                   (print-json V)
                   (print-json V Ind++)))
               (prin (when (cdr X) ",") Newline))
             Jval)
        (prin IndStr "}")))))

