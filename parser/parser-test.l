(load+ "#parser/parser.l" "#lib/unit.l")
#(symbols 'pico 'parser 'unit)

# error test helper
(de err-help (R C Label X)
  (let Inp (new '(+PInp) NIL (new '(+PPos) R C))
    (m> (label Label
          (if X
            (err-parse Inp X)
            (err-no-inp Inp))))))

(deftest t-charp ()
  (let pA (charp "A")
    (check=
      ("A" (parse pA "A"))
      (T (error? (parse pA "B")))
      ((err-help 1 1 "char 'A'" "B") (parse pA "B"))
      (T (error? (parse pA (pinp ""))))
      ((err-help 1 1 "char 'A'") (parse pA ""))
      )))

(deftest t-digitp ()
  (check=
    ("1" (parse 'digitp "1"))
    ))

(deftest t-wsp ()
  (check=
    (" " (parse 'space " "))
    ))

(deftest t-lowcp ()
  (check=
    ("g" (parse 'lowcp "g"))
    ))

(deftest t-uppcp ()
  (check=
    ("G" (parse (uppcp) "G"))
    ))

(deftest t-alphap ()
  (check=
    ("1" (parse (alphap) "1"))
    ("g" (parse (alphap) "g"))
    ("G" (parse (alphap) "G"))
    ))

(deftest t-hexp ()
  (check=
    ("1" (parse (hexp) "1"))
    ("c" (parse (hexp) "c"))
    ("C" (parse (hexp) "C"))
    ))

(deftest t-andp ()
  (let (pA (charp "A")
        pB (charp "B")
        pC (charp "C")
        pAB (andp 'pA 'pB)
        pBC (andp 'pB 'pC)
        pABC (andp 'pAB 'pC)
        pABC2 (andp 'pA 'pBC))
    (check=
      ('("A" "B") (parse pAB "AB"))
      ('(("A" "B") "C") (parse pABC "ABC"))
      ('("A" ("B" "C")) (parse pABC2 "ABC"))
      )))

(deftest t-orp ()
  (let (pA (charp "A")
        pB (charp "B")
        pAB (orp pA pB))
    (check=
      ("A" (parse pAB "A"))
      ("B" (parse pAB "B"))
      ("A" (parse pAB "A."))
      )))

(deftest t-sequencep ()
  (let (pA (charp "A")
        pB (charp "B")
        s0 (sequencep)
        sA (sequencep pA)
        sAB (sequencep pA pB))
    (check=
      ('() (parse s0 "A"))
      ('("A") (parse sA "A"))
      (T (perr? (parse sAB "A")))
      ((err-help 1 1 "char")
       (parse sAB "A"))
      ('("A" "B") (parse sAB "AB"))
      ('("A" "B") (parse sAB "ABC"))
      (T (perr? (parse sAB "B")))
      ((err-help 1 1 "char" "B")
       (parse sAB "B"))
      )))

(deftest t-choicep ()
  (let (pA (charp "A")
        pB (charp "B")
        c0 (orp)
        cA (orp pA)
        cAB (orp pA pB))
    (check=
#      ('() (parse c0 "A"))
#      ("A" (parse cA "A"))
#      (T (perr? (parse cA "B")))
#      ((err-help 1 1 "char" "B")
#       (parse cA "B"))
      ("A" (parse cAB "A"))
      ("B" (parse cAB "B"))
      (T (perr? (parse cAB "C")))
      ((err-help 1 1 "char" "C")
       (parse cAB "C"))
      )))

(deftest t-stringp ()
  (let (pABC (stringp "ABC"))
    (check=
      ("ABC" (parse pABC "ABC"))
      ("ABC" (parse pABC "ABC."))
      (T (perr? (parse pABC "AB.")))
      ((err-help 1 3 "string 'ABC'" ".")
       (parse pABC "AB."))
      )))

(deftest t-manyp ()
  (let (pA (charp "A")
        pB (charp "B")
        mA (manyp pA)
        pAB (stringp "AB")
        mAB (manyp pAB)
        mcAB (manyp (orp pA pB)))
    (check=
      ('() (parse mA "B"))
      ('("A") (parse mA "A"))
      ('("A") (parse mA "A."))
      ('("A" "A") (parse mA "AA"))
      ('("A" "A") (parse mA "AA."))
      ('("A" "A" "A") (parse mA "AAA"))
      ('() (parse mAB "B"))
      ('() (parse mAB "AD"))
      ('("AB") (parse mAB "AB"))
      ('("AB") (parse mAB "AB."))
      ('("AB") (parse mAB "AB.AB"))
      ('("AB" "AB") (parse mAB "ABAB."))
      ('("A" "B" "A" "B") (parse mcAB "ABAB."))
      )))

(deftest t-many1p ()
  (let (pA (charp "A")
        mA (many1p 'pA))
    (check=
      (T (perr? (parse mA "B")))
      ('("A") (parse mA "A."))
      ('("A") (parse mA "AB."))
      ('("A" "A") (parse mA "AAB."))
      )))

(deftest t-optp ()
  (let (pA (charp "A")
        pB (charp "B")
        oA (optp pA)
        oB (optp pB)
        aop (andp oA pB))
    (check=
      ('() (parse oA "B"))
      ("A" (parse oA "A"))
      ('("A" "B") (parse aop "AB"))
      ('(NIL "B") (parse aop "B"))
      )))

(deftest t-droprp ()
  (let (pdr (droprp (charp "A") (charp ","))
        AB (andp (charp "A") (charp "B"))
        pdr2 (droprp AB (charp ",")))
    (check=
      ("A" (parse pdr "A,"))
      ('("A" "B") (parse pdr2 "AB,"))
      )))

(deftest t-droplp ()
  (let (pdl (droplp (charp ",") (charp "A"))
        AB (andp (charp "A") (charp "B"))
        pdl2 (droplp (charp ",") AB))
    (check=
      ("A" (parse pdl ",A"))
      ('("A" "B") (parse pdl2 ",AB"))
      )))

(deftest t-betweenp ()
  (let (AB (andp (charp "A") (charp "B"))
        btw (betweenp (charp "(") AB (charp ")")))
    (check=
      ('("A" "B") (parse btw "(AB)"))
      )))

(deftest t-sepbyp ()
  (let (sp (sepbyp (digitp) (charp ";"))
        sp2 (sepbyp (orp (charp "a") (charp "b"))
                    (charp ":")))
    (check=
      ('() (parse sp "A."))
      ('("1") (parse sp "1."))
      ('("1" "2") (parse sp "1;2."))
      ('("1" "2" "3") (parse sp "1;2;3."))
      ('("a") (parse sp2 "a"))
      ('("b") (parse sp2 "b"))
      ('("a" "a") (parse sp2 "a:a"))
      ('("b" "b") (parse sp2 "b:b"))
      ('("a" "b") (parse sp2 "a:b"))
      )))

(deftest t-sepby1p ()
  (let sp (sepby1p (digitp) (charp ";"))
    (check=
      (T (perr? (parse sp "A")))
      (T (perr? (parse sp "A.")))
      ('("1") (parse sp "1."))
      ('("1" "2") (parse sp "1;2."))
      ('("1" "2" "3") (parse sp "1;2;3."))
      )))

(-run-tests)
(bye)
