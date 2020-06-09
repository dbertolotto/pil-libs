# combinatory parser

(setq *Lowcase (make (for I 26 (link (char (+ 96 I))))))
(setq *Uppcase (mapcar uppc *Lowcase))
(setq *Digit (mapcar format (range 0 9)))
(setq *Ws (chop " \t\n\r"))
(setq *Alpha (conc *Lowcase *Uppcase *Digit))
(setq *Hex (conc (chop "ABCDEFabcdef") *Digit))

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
  (fill
    '((Inp)
      (let X (car> Inp)
        (cond
          ((= X NIL) (err-no-inp @Label Inp))
          (('@Pred X) (new '(+PSuc) X (cdr> Inp)))
          (T (err-parse @Label Inp X)))))))

(de charp (@C)
  (satisfy (fill '((X) (= X @C))) "char"))

(let pA (charp "A")
  (test "A" (v> (pA (pinp "A"))))
  (test T (perr? (pA (pinp "B"))))
  (test (err-help 1 1 "char" "B") (m> (pA (pinp "B"))))
  (test T (perr? (pA (pinp ""))))
  (test (err-help 1 1 "char") (m> (pA (pinp ""))))
  )

(def 'digitp (satisfy '((X) (member X *Digit)) "digit"))

(test "1" (v> (digitp (pinp "1"))))

(def 'wsp (satisfy '((X) (member X *Ws)) "whitespace"))

(test " " (v> (wsp (pinp " "))))

(def 'lowcp (satisfy '((X) (member X *Lowcase)) "lowercase"))

(test "g" (v> (lowcp (pinp "g"))))

(def 'uppcp (satisfy '((X) (member X *Uppcase)) "uppercase"))

(test "G" (v> (uppcp (pinp "G"))))

(def 'alphap (satisfy '((X) (member X *Alpha)) "alphanumeric"))

(test "G" (v> (alphap (pinp "G"))))
(test "g" (v> (alphap (pinp "g"))))
(test "1" (v> (alphap (pinp "1"))))

(def 'hexp (satisfy '((X) (member X *Hex)) "hexadecimal"))

(de andp-comb (V1 V2)
  (append (if (atom V1) (list V1) V1)
          (if V2 (list V2) V2)))

(de andp (@P1 @P2)
  (fill
    '((Inp)
      (let R1 ('@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 ('@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (andp-comb (v> R1) (v> R2)) (r> R2))
              R2))
          R1)))))

(let (pA (charp "A")
      pB (charp "B")
      pC (charp "C")
      pAB (andp pA pB)
      pABC (andp (andp pA pB) pC))
  (test '("A" "B") (v> (pAB (pinp "AB"))))
  (test '("A" "B" "C") (v> (pABC (pinp "ABC"))))
  )

(de orp (@P1 @P2)
  (fill
    '((Inp)
      (let R1 ('@P1 Inp)
        (if (isa '+PSuc R1)
          R1
          ('@P2 Inp))))))

(let (pA (charp "A")
      pB (charp "B")
      pAB (orp pA pB))
  (test "A" (v> (pAB (pinp "AB"))))
  (test "B" (v> (pAB (pinp "BA"))))
  )

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(de fp (@Fn @P)
  (fill
    '((Inp)
      (let R ('@P Inp)
        (if (isa '+PSuc R)
          (new '(+PSuc) ('@Fn (v> R)) (r> R))
          R)))))

(de mapp (@Fn P)
  (fp (fill '((X) (mapcar '@Fn X))) P))

(de applyp (@Fn P)
  (fp (fill '((X) (apply '@Fn X))) P))

(de sequencep @
  (reduce 'andp (rest)))

(de choicep @
  (reduce 'orp (rest)))

(de stringp (Str)
  (fp 'pack
    (apply 'sequencep (mapcar 'charp (chop Str)))))

(let abc (stringp "abc")
  (test "abc" (v> (abc (pinp "abcd"))))
  )

(de manyp-comb (V1 V2)
  (insert 0 V2 V1))

(de manyp (@P)
  (fill
    '((Inp)
       (let R1 ('@P Inp)
         (if (isa '+PSuc R1)
           (let R2 ((manyp '@P) (r> R1))
             (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
           (new '(+PSuc) '() Inp))))))

(let (pA (charp "A")
      mA (manyp pA))
  (test '() (v> (mA (pinp "B"))))
  (test '("A") (v> (mA (pinp "A"))))
  (test '("A") (v> (mA (pinp "A."))))
  (test '("A" "A") (v> (mA (pinp "AA"))))
  (test '("A" "A") (v> (mA (pinp "AA."))))
  (test '("A" "A" "A") (v> (mA (pinp "AAA"))))
  )

(let (pAB (stringp "AB")
      mAB (manyp pAB))
  (test '() (v> (mAB (pinp "B"))))
  (test '() (v> (mAB (pinp "AD"))))
  (test '("AB") (v> (mAB (pinp "AB."))))
  (test '("AB" "AB") (v> (mAB (pinp "ABAB."))))
  )

(de many1p (P)
  (fp '((X) (cons (car X) (cadr X)))
      (andp P (manyp P))))

(let (pA (charp "A")
      mA (many1p pA))
  (test T (perr? (mA (pinp "B"))))
  (test '("A") (v> (mA (pinp "A."))))
  (test '("A") (v> (mA (pinp "AB."))))
  (test '("A" "A") (v> (mA (pinp "AAB."))))
  )

(de optp (@P)
  (fill
    '((Inp)
      (let R ('@P Inp)
        (if (isa '+PSuc R)
          R
          (new '(+PSuc) '() Inp))))))

(let (pA (charp "A")
      pB (charp "B")
      oA (optp pA)
      oB (optp pB)
      aop (andp oA pB))
  (test '() (v> (oA (pinp "B"))))
  (test "A" (v> (oA (pinp "A"))))
  (test '("A" "B") (v> (aop (pinp "AB"))))
  (test '(NIL "B") (v> (aop (pinp "B"))))
  )

(let (mP (many1p 'digitp)
      strp (fp 'list (fp 'pack mP))
      nump (fp 'list (fp 'format mP))
      numlp (mapp 'format mP))
  (test '("123") (v> (strp (pinp "123"))))
  (test '(123) (v> (nump (pinp "123"))))
  (test '(1 2 3) (v> (numlp (pinp "123"))))
  )

(de droprp (@P1 @P2)
  (fill
    '((Inp)
      (let R1 ('@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 ('@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (v> R1) (r> R2))
              R2))
          R1)))))

(de droplp (@P1 @P2)
  (fill
    '((Inp)
      (let R1 ('@P1 Inp)
        (if (isa '+PSuc R1)
          (let R2 ('@P2 (r> R1))
            (if (isa '+PSuc R2)
              (new '(+PSuc) (v> R2) (r> R2))
              R2))
          R1)))))

(de betweenp (P1 P2 P3)
  (droprp (droplp P1 P2) P3))

(let (pdr (droprp (charp "A") (charp ","))
      pdl (droplp (charp ",") (charp "A"))
      AB (andp (charp "A") (charp "B"))
      pdr2 (droprp AB (charp ","))
      pdl2 (droplp (charp ",") AB)
      btw (betweenp (charp "(") AB (charp ")")))
  (test "A" (v> (pdr (pinp "A,"))))
  (test "A" (v> (pdl (pinp ",A"))))
  (test '("A" "B") (v> (pdr2 (pinp "AB,"))))
  (test '("A" "B") (v> (pdl2 (pinp ",AB"))))
  (test '("A" "B") (v> (btw (pinp "(AB)"))))
  )

(de sepbyp (@P @Psep)
  (fill
    '((Inp)
       (let R1 ('@P Inp)
         (if (isa '+PSuc R1)
           (let R2 ((manyp (droplp '@Psep '@P)) (r> R1))
             (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
           (new '(+PSuc) '() Inp))))))

(let sp (sepbyp 'digitp (charp ";"))
  (test '() (v> (sp (pinp "A."))))
  (test '("1") (v> (sp (pinp "1."))))
  (test '("1" "2") (v> (sp (pinp "1;2."))))
  (test '("1" "2" "3") (v> (sp (pinp "1;2;3."))))
  )

(de sepby1p (@P @Psep)
  (fill
    '((Inp)
       (let R1 ('@P Inp)
         (if (isa '+PSuc R1)
           (let R2 ((manyp (droplp '@Psep '@P)) (r> R1))
             (new '(+PSuc) (manyp-comb (v> R1) (v> R2)) (r> R2)))
           R1)))))

(let sp (sepby1p 'digitp (charp ";"))
  (test T (perr? (sp (pinp "A"))))
  (test T (perr? (sp (pinp "A."))))
  (test '("1") (v> (sp (pinp "1."))))
  (test '("1" "2") (v> (sp (pinp "1;2."))))
  (test '("1" "2" "3") (v> (sp (pinp "1;2;3."))))
  )

(def 'intp
  (fp 'list
    (fp 'format
      (andp (optp (orp (charp "+") (charp "-")))
            (many1p 'digitp)))))

(test '(123) (v> (intp (pinp "123A"))))
(test '(123) (v> (intp (pinp "+123A"))))
(test '(-123) (v> (intp (pinp "-123A"))))

###

(de ignorep (@P @R)
  (fill
    '((Inp)
      (let R ('@P Inp)
        (if (isa '+PSuc R)
          (new '(+PSuc) @R (r> R))
          R)))))

(def 'jnull (fp 'any (stringp "null")))
(def 'jtrue (fp 'any (stringp "true")))
(def 'jfalse (fp 'any (stringp "false")))
(def 'jbool (orp 'jtrue 'jfalse))

(test 'null (v> (jnull (pinp "null"))))
(test 'true (v> (jtrue (pinp "true"))))
(test 'false (v> (jfalse (pinp "false"))))

(def 'junescaped
  (satisfy '((X) (not (or (= X "\\") (= X "\""))))))

(test "a" (v> (junescaped (pinp "a"))))
(test T (perr? (junescaped (pinp "\\"))))

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

(test "\\" (v> (jescaped (pinp "\\\\"))))
(test "\t" (v> (jescaped (pinp "\\t"))))

(def 'junicode
  (let (bl (charp "\\")
        uc (charp "u")
        hx (sequencep 'hexp 'hexp 'hexp 'hexp)
        code (fp 'char (fp 'hex (fp 'pack hx))))
    (droplp bl (droplp uc code))))

(test (char (hex "263A")) (v> (junicode (pinp "\\u263A"))))

(def 'jchar (choicep 'junescaped 'jescaped 'junicode))

(test "a" (v> (jchar (pinp "ab"))))
(test "\\" (v> (jchar (pinp "\\\\"))))
(test (char (hex "263A")) (v> (jchar (pinp "\\u263A"))))

(let ajc (andp 'jchar 'jchar)
  (test '("a" "\\") (v> (ajc (pinp "a\\\\"))))
  (test '("a" "b") (v> (ajc (pinp "ab"))))
  )

(let ajc (sequencep 'jchar 'jchar)
  (test '("a" "\\") (v> (ajc (pinp "a\\\\"))))
  (test '("a" "b") (v> (ajc (pinp "ab"))))
  )

(let ajc (manyp 'junescaped)
  (test '() (v> (ajc (pinp "\\"))))
  (test '("a") (v> (ajc (pinp "a"))))
  (test '("a" "b") (v> (ajc (pinp "ab"))))
  )

(def 'jmanychar (manyp 'jchar))

(test '("\t") (v> (jmanychar (pinp "\\t"))))
(test '("a" "b") (v> (jmanychar (pinp "ab"))))

(def 'jstring
  (let (quotep (charp "\"")
        manych (fp 'pack (manyp 'jchar)))
    (droprp (droplp quotep manych) quotep)))

(test "" (v> (jstring (pinp "\"\""))))
(test "a" (v> (jstring (pinp "\"a\""))))
(test "ab" (v> (jstring (pinp "\"ab\""))))
(test "ab\tde" (v> (jstring (pinp "\"ab\\tde\""))))
(test "ab?de" (v> (jstring (pinp "\"ab\\u003fde\""))))

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
        fracpart (droplp point (many1p 'digitp))
        exppart (droplp exp intpart)
        tuple (sequencep intpart
                         (optp fracpart)
                         (optp exppart)))
    (applyp 'to-num (mapp 'pack tuple))))

(setq *Scl 0)
(test 0 (v> (jnumber (pinp "0"))))
(setq *Scl 0)
(test 12 (v> (jnumber (pinp "+1.2e-3"))))

(def 'spacesp (manyp 'wsp))

(de to-array (Lst)
  (append '(T) Lst))

(def 'jarray
  (let (left (droprp (charp "[") 'spacesp)
        right (droprp (charp "]") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        value (droprp 'jvalue 'spacesp)
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
  (let (left (droprp (charp "{") 'spacesp)
        right (droprp (charp "}") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        colon (droprp (charp ":") 'spacesp)
        key (droprp 'jstring 'spacesp)
        value (droprp 'jvalue 'spacesp)
        keyval (andp (droprp key colon) value)
        keyvals (sepbyp keyval comma))
    (betweenp left keyvals right)))

(def 'jvalue
  (choicep
    'jobject
    'jarray
    'jstring
    'jnumber
    'jbool
    'jnull))

(setq *Scl 0)
(test '(T) (v> (jarray (pinp "[ ]"))))
(test '(T "a" false 12)
      (v> (jarray (pinp "[ \"a\" , false , 12 ]"))))
(test '(T "ab" (T true 12) "cd")
      (v> (jarray (pinp "[\"ab\",[true,12],\"cd\"]"))))
(test T (perr? (jarray (pinp "[\"a\" \"b\"]"))))
(test (err-help 1 6 NIL "\"") (m> (jarray (pinp "[\"a\" \"b\"]"))))
(test (err-help 2 5 NIL "\"") (m> (jarray (pinp "[\"a\",\n\"b\" \"c\"]"))))
(test (err-help 1 6 NIL "\"") (m> (jarray (pinp "[\"a\",[\"c\"23],23]"))))

(setq *Scl 0)
(test '() (v> (jobject (pinp "{ }"))))
(test '(("a" false))
      (v> (jobject (pinp "{\"a\":false}"))))
(test '(("b" null))
      (v> (jobject (pinp "{ \"b\" : null }"))))
(test '(("b" null) ("c" 12))
      (v> (jobject (pinp "{\"b\":null , \"c\":12}"))))
(test T (perr? (jobject (pinp "{\"b\":null \"c\":12}"))))
(test (err-help 1 11 NIL "\"") (m> (jobject (pinp "{\"b\":null \"c\":12}"))))
(test (err-help 1 11 NIL "\"") (m> (jobject (pinp "{\"a\":{\"b\":null \"c\":12}}"))))

(setq *Scl 0)
(test '(T "a" (("b" false)))
      (v> (jarray (pinp "[\"a\",{\"b\":false}]"))))
(test '(("a" (T false)))
      (v> (jobject (pinp "{\"a\":[false]}"))))
(test '(("a" (T false 1)))
      (v> (jobject (pinp "{\"a\":[false,1]}"))))
(test '(("a" (T false (("b" 1)))))
      (v> (jobject (pinp "{\"a\":[false,{\"b\":1}]}"))))

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
    ((isa '+PInp Source) (jvalue Source))
    ((info Source) (jvalue (to-pinp Source)))
    ((str? Source) (jvalue (new '(+PInp) (chop Source) 1 1)))
    ((lst? Source) (jvalue (new '(+PInp) Source 1 1)))
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
