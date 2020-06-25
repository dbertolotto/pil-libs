(load+ "#parser/parser.l" "#lib/unit.l")
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
      ('(("A" "B") "C") (parsev pABC "ABC"))
      ('("A" ("B" "C")) (parsev pABC2 "ABC"))
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

(deftest t-sequencep ()
  (let (pA (charp "A")
        pB (charp "B")
        s0 (sequencep)
        sA (sequencep pA)
        sAB (sequencep pA pB))
    (check=
      ('() (parsev s0 "A"))
      ('("A") (parsev sA "A"))
      (T (perr? (parse sAB "A")))
      ((err-help 1 1 "char")
       (parsem sAB "A"))
      ('("A" "B") (parsev sAB "AB"))
      ('("A" "B") (parsev sAB "ABC"))
      (T (perr? (parse sAB "B")))
      ((err-help 1 1 "char" "B")
       (parsem sAB "B"))
      )))

(deftest t-choicep ()
  (let (pA (charp "A")
        pB (charp "B")
        c0 (choicep)
        cA (choicep pA)
        cAB (choicep pA pB))
    (check=
#      ('() (parsev c0 "A"))
#      ("A" (parsev cA "A"))
#      (T (perr? (parse cA "B")))
#      ((err-help 1 1 "char" "B")
#       (parsem cA "B"))
      ("A" (parsev cAB "A"))
      ("B" (parsev cAB "B"))
      (T (perr? (parse cAB "C")))
      ((err-help 1 1 "char" "C")
       (parsem cAB "C"))
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

(deftest t-manyp ()
  (let (pA (charp "A")
        pB (charp "B")
        mA (manyp pA)
        pAB (stringp "AB")
        mAB (manyp pAB)
        mcAB (manyp (choicep pA pB)))
    (check=
      ('() (parsev mA "B"))
      ('("A") (parsev mA "A"))
      ('("A") (parsev mA "A."))
      ('("A" "A") (parsev mA "AA"))
      ('("A" "A") (parsev mA "AA."))
      ('("A" "A" "A") (parsev mA "AAA"))
      ('() (parsev mAB "B"))
      ('() (parsev mAB "AD"))
      ('("AB") (parsev mAB "AB"))
      ('("AB") (parsev mAB "AB."))
      ('("AB") (parsev mAB "AB.AB"))
      ('("AB" "AB") (parsev mAB "ABAB."))
      ('("A" "B" "A" "B") (parsev mcAB "ABAB."))
      )))

(deftest t-many1p ()
  (let (pA (charp "A")
        mA (many1p pA))
    (check=
      (T (perr? (parse mA "B")))
      ('("A") (parsev mA "A."))
      ('("A") (parsev mA "AB."))
      ('("A" "A") (parsev mA "AAB."))
      )))

(deftest t-optp ()
  (let (pA (charp "A")
        pB (charp "B")
        oA (optp pA)
        oB (optp pB)
        aop (andp oA pB))
    (check=
      ('() (parsev oA "B"))
      ("A" (parsev oA "A"))
      ('("A" "B") (parsev aop "AB"))
      ('(NIL "B") (parsev aop "B"))
      )))

(deftest t-droprp ()
  (let (pdr (droprp (charp "A") (charp ","))
        AB (andp (charp "A") (charp "B"))
        pdr2 (droprp AB (charp ",")))
    (check=
      ("A" (parsev pdr "A,"))
      ('("A" "B") (parsev pdr2 "AB,"))
      )))

(deftest t-droplp ()
  (let (pdl (droplp (charp ",") (charp "A"))
        AB (andp (charp "A") (charp "B"))
        pdl2 (droplp (charp ",") AB))
    (check=
      ("A" (parsev pdl ",A"))
      ('("A" "B") (parsev pdl2 ",AB"))
      )))

(deftest t-betweenp ()
  (let (AB (andp (charp "A") (charp "B"))
        btw (betweenp (charp "(") AB (charp ")")))
    (check=
      ('("A" "B") (parsev btw "(AB)"))
      )))

(deftest t-sepbyp ()
  (let (sp (sepbyp digitp (charp ";"))
        sp2 (sepbyp (orp (charp "a") (charp "b"))
                    (charp ":")))
    (check=
      ('() (parsev sp "A."))
      ('("1") (parsev sp "1."))
      ('("1" "2") (parsev sp "1;2."))
      ('("1" "2" "3") (parsev sp "1;2;3."))
      ('("a") (parsev sp2 "a"))
      ('("b") (parsev sp2 "b"))
      ('("a" "a") (parsev sp2 "a:a"))
      ('("b" "b") (parsev sp2 "b:b"))
      ('("a" "b") (parsev sp2 "a:b"))
      )))

(deftest t-sepby1p ()
  (let sp (sepby1p digitp (charp ";"))
    (check=
      (T (perr? (parse sp "A")))
      (T (perr? (parse sp "A.")))
      ('("1") (parsev sp "1."))
      ('("1" "2") (parsev sp "1;2."))
      ('("1" "2" "3") (parsev sp "1;2;3."))
      )))

(deftest t-parse ()
  (combine-results
    (t-satisfy)
    (t-andp)
    (t-orp)
    (t-sequencep)
    (t-choicep)
    (t-stringp)
    (t-manyp)
    (t-many1p)
    (t-optp)
    (t-droprp)
    (t-droplp)
    (t-betweenp)
    (t-sepbyp)
    (t-sepby1p)
    ))

(t-parse)

(let (mP (many1p digitp)
      strp (fp 'list (fp 'pack mP))
      nump (fp 'list (fp 'format mP))
      numlp (mapp 'format mP))
  (test '("123") (parsev strp "123"))
  (test '(123) (parsev nump "123"))
  (test '(1 2 3) (parsev numlp "123"))
  )

(test '(123) (parsev intp "123A"))
(test '(123) (parsev intp "+123A"))
(test '(-123) (parsev intp "-123A"))

(bye)
