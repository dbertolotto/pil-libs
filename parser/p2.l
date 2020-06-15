# combinatory parser

(setq *Lowcase (make (for I 26 (link (char (+ 96 I))))))
(setq *Uppcase (mapcar uppc *Lowcase))
(setq *Digit (mapcar format (range 0 9)))
(setq *Ws (chop " \t\n\r"))
(setq *Alpha (conc *Lowcase *Uppcase *Digit))
(setq *Hex (conc (chop "ABCDEFabcdef") *Digit))

(class +Parser)
(dm T (Fun Label)
  (=: Fun Fun)
  (=: Label Label))
(dm parse> (Inp)
  ((: Fun) Inp))

(class +PSuc)
(dm T (V RInp)
  (=: V V)
  (=: RInp RInp))
(dm v> () (: V))
(dm r> () (: RInp))

(class +PErr)
(dm T (Label Err Pos)
  (=: Label Label)
  (=: Err Err)
  (=: Pos Pos))
(dm l> (NewLabel)
  (put This 'Label NewLabel)
  This)
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

(class +PInp)
(dm T (Inp Pos)
  (=: Inp Inp)
  (=: Pos Pos))
(dm pos> () (: Pos))
(dm car> () (car (: Inp)))
(dm cdr> ()
  (let (Next (cadr (: Inp))
        R (if (= Next "\n") (r+> (: Pos)) (r> (: Pos)))
        C (cond
            ((= Next "\n") 0)
            ((= Next NIL) (c> (: Pos)))
            (T (c+> (: Pos)))))
    (prinl R " " C " " (cdr (: Inp)))
    (new '(+PInp) (cdr (: Inp)) (new '(+PPos) R C))))

# helper function for tests
(de pinp (Str)
  (new '(+PInp) (chop Str) (new '(+PPos) 1 1)))

# helper function for tests
(de perr? (PRes)
  (not (= NIL (isa '+PErr PRes))))

(de err-parse (Label Inp X)
  (new '(+PErr) Label (text "Unexpected '@1'" X) (pos> Inp)))

(de err-no-inp (Label Inp)
  (new '(+PErr) Label "No more Input" (pos> Inp)))

# test helper
(de err-help (R C Label X)
  (let Inp (new '(+PInp) NIL (new '(+PPos) R C))
    (m> (if X
          (err-parse Label Inp X)
          (err-no-inp Label Inp)))))

(de satisfy (@Pred @Label)
  (new '(+Parser)
  (fill
    '((Inp)
      (let X (car> Inp)
        (cond
          ((= X NIL) (err-no-inp @Label Inp))
          (('@Pred X) (new '(+PSuc) X (cdr> Inp)))
          (T (err-parse @Label Inp X))))))
  @Label))

(de charp (@C)
  (satisfy (fill '((X) (= X @C))) "char"))

(let pA (charp "A")
  (test "A" (v> (parse> pA (pinp "A"))))
  (test T (perr? (parse> pA (pinp "B"))))
  (test (err-help 1 1 "char" "B") (m> (parse> pA (pinp "B"))))
  (test T (perr? (parse> pA (pinp ""))))
  (test (err-help 1 1 "char") (m> (parse> pA (pinp ""))))
  )

(def 'digitp (satisfy '((X) (member X *Digit)) "digit"))

(let pDig digitp
  (test "1" (v> (parse> pDig (pinp "1")))))

(def 'wsp (satisfy '((X) (member X *Ws)) "whitespace"))

(let pWs wsp
  (test " " (v> (parse> pWs (pinp " ")))))

(def 'lowcp (satisfy '((X) (member X *Lowcase)) "lowercase"))

(let pLowc lowcp
  (test "g" (v> (parse> pLowc (pinp "g")))))

(def 'uppcp (satisfy '((X) (member X *Uppcase)) "uppercase"))

(let pUppc uppcp
  (test "G" (v> (parse> pUppc (pinp "G")))))

(def 'alphap (satisfy '((X) (member X *Alpha)) "alphanumeric"))

(let pAlpha alphap
  (test "G" (v> (parse> pAlpha (pinp "G"))))
  (test "g" (v> (parse> pAlpha (pinp "g"))))
  (test "1" (v> (parse> pAlpha (pinp "1")))))

(def 'hexp (satisfy '((X) (member X *Hex)) "hexadecimal"))

(de andp-comb (V1 V2)
  (append (if (atom V1) (list V1) V1)
          (if V2 (list V2) V2)))

(de andp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (parse> '@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 (parse> '@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (andp-comb (v> R1) (v> R2)) (r> R2))
              R2))
          R1))))))

(let (pA (charp "A")
      pB (charp "B")
      pC (charp "C")
      pAB (andp pA pB)
      pABC (andp pAB pC))
  (test '("A" "B") (v> (parse> pAB (pinp "AB"))))
  (test '("A" "B" "C") (v> (parse> pABC (pinp "ABC"))))
  )

(de orp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (parse> '@P1 Inp)
        (if (isa '+PSuc R1)
          R1
          (parse> '@P2 Inp)))))))

(let (pA (charp "A")
      pB (charp "B")
      pAB (orp pA pB))
  (test "A" (v> (parse> pAB (pinp "AB"))))
  (test "B" (v> (parse> pAB (pinp "BA"))))
  )

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(de fp (@Fn @P @Label)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R (parse> '@P Inp)
        (if (isa '+PSuc R)
          (new '(+PSuc) ('@Fn (v> R)) (r> R))
          (l> R @Label)))))
  @Label))

(de mapp (@Fn P Label)
  (fp (fill '((X) (mapcar '@Fn X))) P Label))

(de applyp (@Fn P Label)
  (fp (fill '((X) (apply '@Fn X))) P Label))

(de sequencep @
  (reduce 'andp (rest)))

(de choicep @
  (reduce 'orp (rest)))

(de stringp (Str)
  (fp 'pack
    (apply 'sequencep (mapcar 'charp (chop Str))) Str))

(let abc (stringp "abc")
  (test "abc" (v> (parse> abc (pinp "abcd"))))
  (test (err-help 1 3 "abc" "d") (m> (parse> abc (pinp "abd"))))
  )

(de manyp-comb (V1 V2)
  (insert 0 V2 V1))

(de manyp (@P)
  (new '(+Parser)
  (fill
    '((Inp)
       (let R1 (parse> '@P Inp)
         (if (isa '+PSuc R1)
           (let R2 (parse> (manyp '@P) (r> R1))
             (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
           (new '(+PSuc) '() Inp)))))))

(let (pA (charp "A")
      mA (manyp pA))
  (test '() (v> (parse> mA (pinp "B"))))
  (test '("A") (v> (parse> mA (pinp "A"))))
  (test '("A") (v> (parse> mA (pinp "A."))))
  (test '("A" "A") (v> (parse> mA (pinp "AA"))))
  (test '("A" "A") (v> (parse> mA (pinp "AA."))))
  (test '("A" "A" "A") (v> (parse> mA (pinp "AAA"))))
  )

(let (pAB (stringp "AB")
      mAB (manyp pAB))
  (test '() (v> (parse> mAB (pinp "B"))))
  (test '() (v> (parse> mAB (pinp "AD"))))
  (test '("AB") (v> (parse> mAB (pinp "AB."))))
  (test '("AB" "AB") (v> (parse> mAB (pinp "ABAB."))))
  )

(de many1p (P)
  (fp '((X) (cons (car X) (cadr X)))
      (andp P (manyp P))))

(let (pA (charp "A")
      mA (many1p pA))
  (test T (perr? (parse> mA (pinp "B"))))
  (test '("A") (v> (parse> mA (pinp "A."))))
  (test '("A") (v> (parse> mA (pinp "AB."))))
  (test '("A" "A") (v> (parse> mA (pinp "AAB."))))
  )

(de optp (@P)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R (parse> '@P Inp)
        (if (isa '+PSuc R)
          R
          (new '(+PSuc) '() Inp)))))))

(let (pA (charp "A")
      pB (charp "B")
      oA (optp pA)
      oB (optp pB)
      aop (andp oA pB))
  (test '() (v> (parse> oA (pinp "B"))))
  (test "A" (v> (parse> oA (pinp "A"))))
  (test '("A" "B") (v> (parse> aop (pinp "AB"))))
  (test '(NIL "B") (v> (parse> aop (pinp "B"))))
  )

(let (mP (many1p digitp)
      strp (fp 'list (fp 'pack mP))
      nump (fp 'list (fp 'format mP))
      numlp (mapp 'format mP))
  (test '("123") (v> (parse> strp (pinp "123"))))
  (test '(123) (v> (parse> nump (pinp "123"))))
  (test '(1 2 3) (v> (parse> numlp (pinp "123"))))
  )

(de droprp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (parse> '@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 (parse> '@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (v> R1) (r> R2))
              R2))
          R1))))))

(de droplp (@P1 @P2)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R1 (parse> '@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 (parse> '@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (v> R2) (r> R2))
              R2))
          R1))))))

(de betweenp (P1 P2 P3)
  (droprp (droplp P1 P2) P3))

(let (pdr (droprp (charp "A") (charp ","))
      pdl (droplp (charp ",") (charp "A"))
      AB (andp (charp "A") (charp "B"))
      pdr2 (droprp AB (charp ","))
      pdl2 (droplp (charp ",") AB)
      btw (betweenp (charp "(") AB (charp ")")))
  (test "A" (v> (parse> pdr (pinp "A,"))))
  (test "A" (v> (parse> pdl (pinp ",A"))))
  (test '("A" "B") (v> (parse> pdr2 (pinp "AB,"))))
  (test '("A" "B") (v> (parse> pdl2 (pinp ",AB"))))
  (test '("A" "B") (v> (parse> btw (pinp "(AB)"))))
  )

(de sepbyp (@P @Psep)
  (new '(+Parser)
  (fill
    '((Inp)
       (let R1 (parse> '@P Inp)
         (if (isa '+PSuc R1)
           (let R2 (parse> (manyp (droplp '@Psep '@P)) (r> R1))
             (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
           (new '(+PSuc) '() Inp)))))))

(let sp (sepbyp digitp (charp ";"))
  (test '() (v> (parse> sp (pinp "A."))))
  (test '("1") (v> (parse> sp (pinp "1."))))
  (test '("1" "2") (v> (parse> sp (pinp "1;2."))))
  (test '("1" "2" "3") (v> (parse> sp (pinp "1;2;3."))))
  )

(de sepby1p (@P @Psep)
  (new '(+Parser)
  (fill
    '((Inp)
       (let R1 (parse> '@P Inp)
         (if (isa '+PSuc R1)
           (let R2 (parse> (manyp (droplp '@Psep '@P)) (r> R1))
             (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
           R1))))))

(let sp (sepby1p digitp (charp ";"))
  (test T (perr? (parse> sp (pinp "A"))))
  (test T (perr? (parse> sp (pinp "A."))))
  (test '("1") (v> (parse> sp (pinp "1."))))
  (test '("1" "2") (v> (parse> sp (pinp "1;2."))))
  (test '("1" "2" "3") (v> (parse> sp (pinp "1;2;3."))))
  )

(def 'intp
  (fp 'list
    (fp 'format
      (andp (optp (orp (charp "+") (charp "-")))
            (many1p digitp)))))

(test '(123) (v> (parse> intp (pinp "123A"))))
(test '(123) (v> (parse> intp (pinp "+123A"))))
(test '(-123) (v> (parse> intp (pinp "-123A"))))

###

(de ignorep (@P @R)
  (new '(+Parser)
  (fill
    '((Inp)
      (let R (parse> '@P Inp)
        (if (isa '+PSuc R)
          (new '(+PSuc) @R (r> R))
          R))))))

(def 'jnull (fp 'any (stringp "null")))
(def 'jtrue (fp 'any (stringp "true")))
(def 'jfalse (fp 'any (stringp "false")))
(def 'jbool (orp 'jtrue 'jfalse))

(test 'null (v> (parse> jnull (pinp "null"))))
(test 'true (v> (parse> jtrue (pinp "true"))))
(test 'false (v> (parse> jfalse (pinp "false"))))

(def 'junescaped
  (satisfy '((X) (not (or (= X "\\") (= X "\""))))))

(test "a" (v> (parse> junescaped (pinp "a"))))
(test T (perr? (parse> junescaped (pinp "\\"))))

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

(test "\\" (v> (parse> jescaped (pinp "\\\\"))))
(test "\t" (v> (parse> jescaped (pinp "\\t"))))

(def 'junicode
  (let (bl (charp "\\")
        uc (charp "u")
        hx (sequencep hexp hexp hexp hexp)
        code (fp 'char (fp 'hex (fp 'pack hx))))
    (droplp bl (droplp uc code))))

(test (char (hex "263A")) (v> (parse> junicode (pinp "\\u263A"))))

(def 'jchar (choicep junescaped jescaped junicode))

(test "a" (v> (parse> jchar (pinp "ab"))))
(test "\\" (v> (parse> jchar (pinp "\\\\"))))
(test (char (hex "263A")) (v> (parse> jchar (pinp "\\u263A"))))

(let ajc (andp jchar jchar)
  (test '("a" "\\") (v> (parse> ajc (pinp "a\\\\"))))
  (test '("a" "b") (v> (parse> ajc (pinp "ab"))))
  )

(let ajc (sequencep jchar jchar)
  (test '("a" "\\") (v> (parse> ajc (pinp "a\\\\"))))
  (test '("a" "b") (v> (parse> ajc (pinp "ab"))))
  )

(let ajc (manyp junescaped)
  (test '() (v> (parse> ajc (pinp "\\"))))
  (test '("a") (v> (parse> ajc (pinp "a"))))
  (test '("a" "b") (v> (parse> ajc (pinp "ab"))))
  )

(def 'jmanychar (manyp jchar))

(test '("\t") (v> (parse> jmanychar (pinp "\\t"))))
(test '("a" "b") (v> (parse> jmanychar (pinp "ab"))))

(def 'jstring
  (let (quotep (charp "\"")
        manych (fp 'pack (manyp jchar)))
    (droprp (droplp quotep manych) quotep)))

(test "" (v> (parse> jstring (pinp "\"\""))))
(test "a" (v> (parse> jstring (pinp "\"a\""))))
(test "ab" (v> (parse> jstring (pinp "\"ab\""))))
(test "ab\tde" (v> (parse> jstring (pinp "\"ab\\tde\""))))
(test "ab?de" (v> (parse> jstring (pinp "\"ab\\u003fde\""))))

(de to-num (s int fr se ex)
  (let (exn (format ex)
        exval (if exn (apply * (need exn 10)) 1)
        exlen (if (= se "-") exn 0)
        frlen (length fr))
    (setq *Scl (max *Scl (+ frlen exlen)))
    (let (num (format (pack s int "." fr) *Scl)
          op (if (= se "-") '/ '*))
      (op num exval))))

(setq *Scl 0)
(test 1 (to-num "" "1" "" "" ""))
(setq *Scl 0)
(test 1 (to-num "+" "1" "" "" ""))
(setq *Scl 0)
(test -1 (to-num "-" "1" "" "" ""))
(setq *Scl 0)
(test -12 (to-num "-" "1" "2" "" ""))
(setq *Scl 0)
(test -1200 (to-num "-" "1" "2" "" "2"))
(setq *Scl 0)
(test 12 (to-num "" "1" "2" "-" "2"))

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
    (applyp 'to-num (mapp 'pack tuple))))

(setq *Scl 0)
(test 0 (v> (parse> jnumber (pinp "0"))))
(setq *Scl 0)
(test 12 (v> (parse> jnumber (pinp "+1.2e-3"))))

(def 'spacesp (manyp wsp))

(de to-array (Lst)
  (append '(T) Lst))

(def 'jarray
  (let (left (droprp (charp "[") spacesp)
        right (droprp (charp "]") spacesp)
        comma (droprp (charp ",") spacesp)
        value (droprp jvalue spacesp)
        values (sepbyp value comma))
    (fp 'to-array (betweenp left values right))))

# not used
(de odd (Lst)
  (make
    (for (I . X) Lst
      (unless (= 0 (% I 2))
        (link X)))))

# not used
(de even (Lst)
  (make
    (for (I . X) Lst
      (when (= 0 (% I 2))
        (link X)))))

(def 'jobject
  (let (left (droprp (charp "{") spacesp)
        right (droprp (charp "}") spacesp)
        comma (droprp (charp ",") spacesp)
        colon (droprp (charp ":") spacesp)
        key (droprp jstring spacesp)
        value (droprp jvalue spacesp)
        keyval (andp (droprp key colon) value)
        keyvals (sepbyp keyval comma))
    (betweenp left keyvals right)))

(def 'jvalue
  (choicep
    jobject
    jarray
    jstring
    jnumber
    jbool
    jnull))

(setq *Scl 0)
(test '(T) (v> (parse> jarray (pinp "[ ]"))))
(test '(T "a" false 12)
      (v> (parse> jarray (pinp "[ \"a\" , false , 12 ]"))))
(test '(T "ab" (T true 12) "cd")
      (v> (parse> jarray (pinp "[\"ab\",[true,12],\"cd\"]"))))
(test T (perr? (parse> jarray (pinp "[\"a\" \"b\"]"))))
(test (err-help 1 6 NIL "\"") (m> (parse> jarray (pinp "[\"a\" \"b\"]"))))
(test (err-help 2 5 NIL "\"") (m> (parse> jarray (pinp "[\"a\",\n\"b\" \"c\"]"))))
(test (err-help 1 6 NIL "\"") (m> (parse> jarray (pinp "[\"a\",[\"c\"23],23]"))))

(setq *Scl 0)
(test '() (v> (parse> jobject (pinp "{ }"))))
(test '(("a" false))
      (v> (parse> jobject (pinp "{\"a\":false}"))))
(test '(("b" null))
      (v> (parse> jobject (pinp "{ \"b\" : null }"))))
(test '(("b" null) ("c" 12))
      (v> (parse> jobject (pinp "{\"b\":null , \"c\":12}"))))
(test T (perr? (parse> jobject (pinp "{\"b\":null \"c\":12}"))))
(test (err-help 1 11 NIL "\"") (m> (parse> jobject (pinp "{\"b\":null \"c\":12}"))))
(test (err-help 1 11 NIL "\"") (m> (parse> jobject (pinp "{\"a\":{\"b\":null \"c\":12}}"))))

(setq *Scl 0)
(test '(T "a" (("b" false)))
      (v> (parse> jarray (pinp "[\"a\",{\"b\":false}]"))))
(test '(("a" (T false)))
      (v> (parse> jobject (pinp "{\"a\":[false]}"))))
(test '(("a" (T false 1)))
      (v> (parse> jobject (pinp "{\"a\":[false,1]}"))))
(test '(("a" (T false (("b" 1)))))
      (v> (parse> jobject (pinp "{\"a\":[false,{\"b\":1}]}"))))

(de to-chars (file)
  (chop (in file (till NIL T))))

(de to-pinp (file)
  (new '(+PInp) (to-chars file) 1 1))

(setq ex01 (to-pinp "ex01.json"))
(setq ex02 (to-pinp "ex02.json"))
(setq ex03 (to-pinp "ex03.json"))
(setq ex04 (to-pinp "ex04.json"))
(setq ex05 (to-pinp "ex05.json"))

(de read-json (Source)
  (cond
    ((isa '+PInp Source) (parse> jvalue Source))
    ((info Source) (parse> jvalue (to-pinp Source)))
    ((str? Source) (parse> jvalue (new '(+PInp) (chop Source) 1 1)))
    ((lst? Source) (parse> jvalue (new '(+PInp) Source 1 1)))
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

# helper fun for testing
(de to-str (Jval)
  (pipe (print-json Jval) (line T)))

(test "[]" (to-str '(T)))
(test "[\"a\"]" (to-str '(T "a")))
(test "[\"a\",\"b\"]" (to-str '(T "a" "b")))
(test "[1,2]" (to-str '(T 1 2)))

(test "{}" (to-str '()))
(test "{\"a\":false}" (to-str '(("a" false))))
(test "{\"a\":false,\"b\":12}"
      (to-str '(("a" false) ("b" 12))))

(setq *Json-pretty-print T)
