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
(dm T (Msg)
  (=: Msg Msg))
(dm m> () (: Msg))

(de satisfy (@Pred)
  (fill
    '((Inp)
      (let X (car Inp)
        (cond
          ((= X NIL) (new '(+PErr) "No more input"))
          (('@Pred X) (new '(+PSuc) X (cdr Inp)))
          (T (new '(+PErr) (text "Unexpected '@1'" X))))))))

(de charp (@C)
  (satisfy (fill '((X) (= X @C)))))

(let pA (charp "A")
  (test "A" (v> (pA '("A"))))
  (test T (not (= NIL (isa '+PErr (pA '("B"))))))
  (test T (not (= NIL (isa '+PErr (pA '(""))))))
  )

(def 'digitp (satisfy '((X) (member X *Digit))))

(test "1" (v> (digitp '("1"))))

(def 'wsp (satisfy '((X) (member X *Ws))))

(test " " (v> (wsp '(" "))))

(def 'lowcp (satisfy '((X) (member X *Lowcase))))

(test "g" (v> (lowcp '("g"))))

(def 'uppcp (satisfy '((X) (member X *Uppcase))))

(test "G" (v> (uppcp '("G"))))

(def 'alphap (satisfy '((X) (member X *Alpha))))

(test "G" (v> (alphap '("G"))))
(test "g" (v> (alphap '("g"))))
(test "1" (v> (alphap '("1"))))

(def 'hexp (satisfy '((X) (member X *Hex))))

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
  (test '("A" "B") (v> (pAB (chop "AB"))))
  (test '("A" "B" "C") (v> (pABC (chop "ABC"))))
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
  (test "A" (v> (pAB (chop "AB"))))
  (test "B" (v> (pAB (chop "BA"))))
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
  (test "abc" (v> (abc (chop "abcd"))))
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
  (test '() (v> (mA (chop "B"))))
  (test '("A") (v> (mA (chop "A"))))
  (test '("A") (v> (mA (chop "A."))))
  (test '("A" "A") (v> (mA (chop "AA"))))
  (test '("A" "A") (v> (mA (chop "AA."))))
  (test '("A" "A" "A") (v> (mA (chop "AAA"))))
  )

(let (pAB (stringp "AB")
      mAB (manyp pAB))
  (test '() (v> (mAB (chop "B"))))
  (test '() (v> (mAB (chop "AD"))))
  (test '("AB") (v> (mAB (chop "AB."))))
  (test '("AB" "AB") (v> (mAB (chop "ABAB."))))
  )

(de many1p (P)
  (fp '((X) (cons (car X) (cadr X)))
      (andp P (manyp P))))

(let (pA (charp "A")
      mA (many1p pA))
  (test T (not (= NIL (isa '+PErr (mA (chop "B"))))))
  (test '("A") (v> (mA (chop "A."))))
  (test '("A") (v> (mA (chop "AB."))))
  (test '("A" "A") (v> (mA (chop "AAB."))))
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
  (test '() (v> (oA (chop "B"))))
  (test "A" (v> (oA (chop "A"))))
  (test '("A" "B") (v> (aop (chop "AB"))))
  (test '(NIL "B") (v> (aop (chop "B"))))
  )

(let (mP (many1p 'digitp)
      strp (fp 'list (fp 'pack mP))
      nump (fp 'list (fp 'format mP))
      numlp (mapp 'format mP))
  (test '("123") (v> (strp (chop "123"))))
  (test '(123) (v> (nump (chop "123"))))
  (test '(1 2 3) (v> (numlp (chop "123"))))
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
  (test "A" (v> (pdr (chop "A,"))))
  (test "A" (v> (pdl (chop ",A"))))
  (test '("A" "B") (v> (pdr2 (chop "AB,"))))
  (test '("A" "B") (v> (pdl2 (chop ",AB"))))
  (test '("A" "B") (v> (btw (chop "(AB)"))))
  )

(de sepbyp (P Psep)
  (manyp (droplp (optp Psep) P)))

(let sp (sepbyp 'digitp (charp ";"))
  (test '() (v> (sp (chop "A."))))
  (test '("1") (v> (sp (chop "1."))))
  (test '("1" "2") (v> (sp (chop "1;2."))))
  (test '("1" "2" "3") (v> (sp (chop "1;2;3."))))
  )

(de sepby1p (P Psep)
  (fp '((X) (cons (car X) (cadr X)))
      (andp P (sepbyp P Psep))))

(let sp (sepby1p 'digitp (charp ";"))
  (test T (not (= NIL (isa '+PErr (sp (chop "A"))))))
  (test T (not (= NIL (isa '+PErr (sp (chop "A."))))))
  (test '("1") (v> (sp (chop "1."))))
  (test '("1" "2") (v> (sp (chop "1;2."))))
  (test '("1" "2" "3") (v> (sp (chop "1;2;3."))))
  )

(def 'intp
  (fp 'list
    (fp 'format
      (andp (optp (orp (charp "+") (charp "-")))
            (many1p 'digitp)))))

(test '(123) (v> (intp (chop "123A"))))
(test '(123) (v> (intp (chop "+123A"))))
(test '(-123) (v> (intp (chop "-123A"))))

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

(test 'null (v> (jnull (chop "null"))))
(test 'true (v> (jtrue (chop "true"))))
(test 'false (v> (jfalse (chop "false"))))

(def 'junescaped
  (satisfy '((X) (not (or (= X "\\") (= X "\""))))))

(test "a" (v> (junescaped (chop "a"))))
(test T (not (= NIL (isa '+PErr (junescaped "\\")))))

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

(test "\\" (v> (jescaped (chop "\\\\"))))
(test "\t" (v> (jescaped (chop "\\t"))))

(def 'junicode
  (let (bl (charp "\\")
        uc (charp "u")
        hx (sequencep 'hexp 'hexp 'hexp 'hexp)
        code (fp 'char (fp 'hex (fp 'pack hx))))
    (droplp bl (droplp uc code))))

(test (char (hex "263A")) (v> (junicode (chop "\\u263A"))))

(def 'jchar (choicep 'junescaped 'jescaped 'junicode))

(test "a" (v> (jchar (chop "ab"))))
(test "\\" (v> (jchar (chop "\\\\"))))
(test (char (hex "263A")) (v> (jchar (chop "\\u263A"))))

(let ajc (andp 'jchar 'jchar)
  (test '("a" "\\") (v> (ajc (chop "a\\\\"))))
  (test '("a" "b") (v> (ajc (chop "ab"))))
  )

(let ajc (sequencep 'jchar 'jchar)
  (test '("a" "\\") (v> (ajc (chop "a\\\\"))))
  (test '("a" "b") (v> (ajc (chop "ab"))))
  )

(let ajc (manyp 'junescaped)
  (test '() (v> (ajc (chop "\\"))))
  (test '("a") (v> (ajc (chop "a"))))
  (test '("a" "b") (v> (ajc (chop "ab"))))
  )

(def 'jmanychar (manyp 'jchar))

(test '("\t") (v> (jmanychar (chop "\\t"))))
(test '("a" "b") (v> (jmanychar (chop "ab"))))

(def 'jstring
  (let (quotep (charp "\"")
        manych (fp 'pack (manyp 'jchar)))
    (droprp (droplp quotep manych) quotep)))

(test "" (v> (jstring (chop "\"\""))))
(test "a" (v> (jstring (chop "\"a\""))))
(test "ab" (v> (jstring (chop "\"ab\""))))
(test "ab\tde" (v> (jstring (chop "\"ab\\tde\""))))
(test "ab?de" (v> (jstring (chop "\"ab\\u003fde\""))))

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
(test 0 (v> (jnumber (chop "0"))))
(setq *Scl 0)
(test 12 (v> (jnumber (chop "+1.2e-3"))))

(def 'spacesp (manyp 'wsp))

(de to-array (Lst)
  (append '(T) Lst))

(def 'jarray
  (let (aleft (droprp (charp "[") 'spacesp)
        aright (droprp (charp "]") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        value (droprp 'jvalue 'spacesp)
        values (sepbyp value comma))
    (fp 'to-array (betweenp aleft values aright))))

(de odd (Lst)
  (make
    (for (I . X) Lst
      (unless (= 0 (% I 2))
        (link X)))))

(de even (Lst)
  (make
    (for (I . X) Lst
      (when (= 0 (% I 2))
        (link X)))))

(def 'jobject
  (let (oleft (droprp (charp "{") 'spacesp)
        oright (droprp (charp "}") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        colon (droprp (charp ":") 'spacesp)
        key (droprp 'jstring 'spacesp)
        value (droprp 'jvalue 'spacesp)
        keyval (andp (droprp key colon) value)
        keyvals (sepbyp keyval comma))
    (betweenp oleft keyvals oright)))

(def 'jvalue
  (choicep
    'jobject
    'jarray
    'jstring
    'jnumber
    'jbool
    'jnull))

(setq *Scl 0)
(test '(T) (v> (jarray (chop "[ ]"))))
(test '(T "a" false 12)
      (v> (jarray (chop "[ \"a\" , false , 12 ]"))))
(test '(T "ab" (T true 12) "cd")
      (v> (jarray (chop "[\"ab\",[true,12],\"cd\"]"))))
(test '() (v> (jobject (chop "{ }"))))
(test '(("a" false))
      (v> (jobject (chop "{\"a\":false}"))))
(test '(("b" null))
      (v> (jobject (chop "{ \"b\" : null }"))))
(test '(("b" null) ("c" 12))
      (v> (jobject (chop "{\"b\":null , \"c\":12}"))))

(test '(T "a" (("b" false)))
      (v> (jarray (chop "[\"a\",{\"b\":false}]"))))
(test '(("a" (T false)))
      (v> (jobject (chop "{\"a\":[false]}"))))
(test '(("a" (T false 1)))
      (v> (jobject (chop "{\"a\":[false,1]}"))))
(test '(("a" (T false (("b" 1)))))
      (v> (jobject (chop "{\"a\":[false,{\"b\":1}]}"))))

(de to-chars (file)
  (chop (in file (till NIL T))))

(setq ex01 (to-chars "ex01.json"))
(setq ex02 (to-chars "ex02.json"))
(setq ex03 (to-chars "ex03.json"))
(setq ex04 (to-chars "ex04.json"))
(setq ex05 (to-chars "ex05.json"))

(de read-json (Source)
  (cond
    ((info Source) (jvalue (to-chars Source)))
    ((str? Source) (jvalue (chop Source)))
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
