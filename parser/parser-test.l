(load "parser.l" "../lib/unit.l")
(symbols 'pico 'parser 'unit)

# helper function for tests
(de perr? (PRes)
  (not (= NIL (isa '+PErr PRes))))

# error test helper
(de err-help (R C Label X)
  (let Inp (new '(+PInp) NIL (new '(+PPos) R C))
    (m> (label Label
          (if X
            (err-parse Inp X)
            (err-no-inp Inp))))))

# helper functions for tests
(de pinp (Str)
  (new '(+PInp) (chop Str) (pos0)))
(de parse (P Inp)
  (parse> P (pinp Inp)))
(de parsev (P Inp)
  (v> (parse P Inp)))
(de parsem (P Inp)
  (m> (parse P Inp)))

(deftest t-charp ()
  (let pA (charp "A")
    (check=
      ("A" (parsev pA "A"))
      (T (perr? (parse pA "B")))
      ((err-help 1 1 "char" "B") (parsem pA "B"))
      (T (perr? (parse pA (pinp ""))))
      ((err-help 1 1 "char") (parsem pA ""))
      )))

(deftest t-digitp ()
  (check=
    ("1" (parsev digitp "1"))
    ))

(deftest t-wsp ()
  (check=
    (" " (parsev wsp " "))
    ))

(deftest t-lowcp ()
  (check=
    ("g" (parsev lowcp "g"))
    ))

(deftest t-uppcp ()
  (check=
    ("G" (parsev uppcp "G"))
    ))

(deftest t-alphap ()
  (check=
    ("1" (parsev alphap "1"))
    ("g" (parsev alphap "g"))
    ("G" (parsev alphap "G"))
    ))

(deftest t-hexp ()
  (check=
    ("1" (parsev hexp "1"))
    ("c" (parsev hexp "c"))
    ("C" (parsev hexp "C"))
    ))

(deftest t-satisfy ()
  (combine-results
    (t-charp)
    (t-digitp)
    (t-wsp)
    (t-lowcp)
    (t-uppcp)
    (t-alphap)
    (t-hexp)
    ))

(deftest t-andp ()
  (let (pA (charp "A")
        pB (charp "B")
        pC (charp "C")
        pAB (andp pA pB)
        pBC (andp pB pC)
        pABC (andp pAB pC)
        pABC2 (andp pA pBC))
    (check=
      ('("A" "B") (parsev pAB "AB"))
      ('("A" "B" "C") (parsev pABC "ABC"))
      ('("A" "B" "C") (parsev pABC2 "ABC"))
      )))

(deftest t-andp ()
  (let (pA (charp "A")
        pB (charp "B")
        pC (charp "C")
        pAB (andp pA pB)
        pBC (andp pB pC)
        pABC (andp pAB pC)
        pABC2 (andp pA pBC))
    (check=
      ('("A" "B") (parsev pAB "AB"))
      ('("A" "B" "C") (parsev pABC "ABC"))
      ('("A" "B" "C") (parsev pABC2 "ABC"))
      )))

(deftest t-orp ()
  (let (pA (charp "A")
        pB (charp "B")
        pAB (orp pA pB))
    (check=
      ("A" (parsev pAB "A"))
      ("B" (parsev pAB "B"))
      ("A" (parsev pAB "A."))
      )))

(deftest t-stringp ()
  (let (pABC (stringp "ABC"))
    (check=
      ("ABC" (parsev pABC "ABC"))
      ("ABC" (parsev pABC "ABC."))
      (T (perr? (parse pABC "AB.")))
      ((err-help 1 3 "string 'ABC'" ".")
       (parsem pABC "AB."))
      )))

(deftest t-parse ()
  (combine-results
    (t-satisfy)
    (t-andp)
    (t-orp)
    (t-stringp)
    ))

(t-parse)

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

(let (pA (charp "A")
      mA (many1p pA))
  (test T (perr? (parse> mA (pinp "B"))))
  (test '("A") (v> (parse> mA (pinp "A."))))
  (test '("A") (v> (parse> mA (pinp "AB."))))
  (test '("A" "A") (v> (parse> mA (pinp "AAB."))))
  )

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

(let (sp (sepbyp digitp (charp ";"))
      sp2 (sepbyp (orp (charp "a") (charp "b")) (charp ":")))
  (test '() (v> (parse> sp (pinp "A."))))
  (test '("1") (v> (parse> sp (pinp "1."))))
  (test '("1" "2") (v> (parse> sp (pinp "1;2."))))
  (test '("1" "2" "3") (v> (parse> sp (pinp "1;2;3."))))
  (test '("a") (v> (parse> sp2 (pinp "a"))))
  (test '("b") (v> (parse> sp2 (pinp "b"))))
  (test '("a" "a") (v> (parse> sp2 (pinp "a:a"))))
  (test '("b" "b") (v> (parse> sp2 (pinp "b:b"))))
  (test '("a" "b") (v> (parse> sp2 (pinp "a:b"))))
  )

(let sp (sepby1p digitp (charp ";"))
  (test T (perr? (parse> sp (pinp "A"))))
  (test T (perr? (parse> sp (pinp "A."))))
  (test '("1") (v> (parse> sp (pinp "1."))))
  (test '("1" "2") (v> (parse> sp (pinp "1;2."))))
  (test '("1" "2" "3") (v> (parse> sp (pinp "1;2;3."))))
  )

(test '(123) (v> (parse> intp (pinp "123A"))))
(test '(123) (v> (parse> intp (pinp "+123A"))))
(test '(-123) (v> (parse> intp (pinp "-123A"))))

### json

(test 'null (v> (parse> jnull (pinp "null"))))
#(test (err-help 1 3 "jnull" "r")
#      (m> (parse> jnull (pinp "nurl"))))
(test 'true (v> (parse> jtrue (pinp "true"))))
#(test (err-help 1 3 "jtrue" "r")
#      (m> (parse> jtrue (pinp "trre"))))
(test 'true (v> (parse> jbool (pinp "true"))))
#(test (err-help 1 3 "jbool" "r")
#      (m> (parse> jbool (pinp "trre"))))
(test 'false (v> (parse> jfalse (pinp "false"))))
(test 'false (v> (parse> jbool (pinp "false"))))

(test "a" (v> (parse> junescaped (pinp "a"))))
(test T (perr? (parse> junescaped (pinp "\\"))))

(test "\\" (v> (parse> jescaped (pinp "\\\\"))))
(test "\t" (v> (parse> jescaped (pinp "\\t"))))

(test (char (hex "263A")) (v> (parse> junicode (pinp "\\u263A"))))

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

(test "" (v> (parse> jstring (pinp "\"\""))))
(test "a" (v> (parse> jstring (pinp "\"a\""))))
(test "ab" (v> (parse> jstring (pinp "\"ab\""))))
(test "ab\tde" (v> (parse> jstring (pinp "\"ab\\tde\""))))
(test "ab?de" (v> (parse> jstring (pinp "\"ab\\u003fde\""))))

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

(setq *Scl 0)
(test 0 (v> (parse> jnumber (pinp "0"))))
(setq *Scl 0)
(test 12 (v> (parse> jnumber (pinp "+1.2e-3"))))

(setq *Scl 0)
(test '(T) (v> (parse> jarray (pinp "[ ]"))))
(test '(T 1) (v> (parse> jarray (pinp "[1]"))))
(test '(T "a" false 12)
      (v> (parse> jarray (pinp "[ \"a\" , false , 12 ]"))))
(test '(T "ab" (T true 12) "cd")
      (v> (parse> jarray (pinp "[\"ab\",[true,12],\"cd\"]"))))
(test T (perr? (parse> jarray (pinp "[\"a\" \"b\"]"))))
#(test (err-help 1 6 "jarray" "\"")
#      (m> (parse> jarray (pinp "[\"a\" \"b\"]"))))
#(test (err-help 2 5 "jarray" "\"")
#      (m> (parse> jarray (pinp "[\"a\",\n\"b\" \"c\"]"))))
(test (err-help 1 6 "jarray" "2")
      (m> (parse> jarray (pinp "[\"a\",[\"c\"23],23]"))))

(setq *Scl 0)
(test '() (v> (parse> jobject (pinp "{ }"))))
(test '(("a" false))
      (v> (parse> jobject (pinp "{\"a\":false}"))))
(test '(("b" null))
      (v> (parse> jobject (pinp "{ \"b\" : null }"))))
(test '(("b" null) ("c" 12))
      (v> (parse> jobject (pinp "{\"b\":null , \"c\":12}"))))
(test T (perr? (parse> jobject (pinp "{\"b\":null \"c\":12}"))))
(test (err-help 1 11 "jobject" "\"") (m> (parse> jobject (pinp "{\"b\":null \"c\":12}"))))
#(test (err-help 1 11 "jobject" "\"") (m> (parse> jobject (pinp "{\"a\":{\"b\":null \"c\":12}}"))))

(setq *Scl 0)
(test '(T "a" (("b" false)))
      (v> (parse> jarray (pinp "[\"a\",{\"b\":false}]"))))
(test '(("a" (T false)))
      (v> (parse> jobject (pinp "{\"a\":[false]}"))))
(test '(("a" (T false 1)))
      (v> (parse> jobject (pinp "{\"a\":[false,1]}"))))
(test '(("a" (T false (("b" 1)))))
      (v> (parse> jobject (pinp "{\"a\":[false,{\"b\":1}]}"))))

(setq ex01 (to-pinp "ex01.json"))
(setq ex02 (to-pinp "ex02.json"))
(setq ex03 (to-pinp "ex03.json"))
(setq ex04 (to-pinp "ex04.json"))
(setq ex05 (to-pinp "ex05.json"))

### print

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
