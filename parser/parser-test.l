(load "parser.l" "../lib/unit.l")
(symbols '(pico unit parser))

(detest t-pchar ()
  (check
    (= '(NIL "No more input" . NIL) (pchar "A" '("")))
    (= '(NIL "No more input" . NIL) (pchar "A" '(NIL)))
    (= '(NIL "No more input" . NIL) (pchar "A" NIL))
    (= '(("A") . ("B" "C")) (pchar "A" (chop "ABC")))
    (= '(("Z") . ("B" "C")) (pchar "Z" (chop "ZBC")))
    (= '(NIL "Expecting 'A'. Got 'B'" . ("B" "C" "D")) (pchar "A" (chop "BCD")))
    ))

(detest t-pchar2 ()
  (let parse-A (pchar2 "A")
    (check
      (= '(NIL "No more input" . NIL) (parse-A '("")))
      (= '(NIL "No more input" . NIL) (parse-A '(NIL)))
      (= '(("A") . ("B" "C")) (parse-A (chop "ABC")))
      (= '(("Z") . ("B" "C")) ((pchar2 "Z") (chop "ZBC")))
      (= '(NIL "Expecting 'A'. Got 'B'" . ("B" "C" "D")) (parse-A (chop "BCD")))
      )))

(detest t-pchar3 ()
  (let parse-A (pchar3 "A")
    (check
      (= '(NIL "No more input" . NIL) (parse-A '("")))
      (= '(NIL "No more input" . NIL) (parse-A '(NIL)))
      (= '(("A") . ("B" "C")) (parse-A (chop "ABC")))
      (= '(("Z") . ("B" "C")) ((pchar3 "Z") (chop "ZBC")))
      (= '(NIL "Expecting 'A'. Got 'B'" . ("B" "C" "D")) (parse-A (chop "BCD")))
      )))

(detest t-and-p ()
  (let (parse-A (pchar2 "A")
        parse-B (pchar2 "B")
        parse-A-then-B (and-p 'parse-A 'parse-B))
    (check
      (= '(NIL "No more input" . NIL) (parse-A-then-B '("")))
      (= '(NIL "No more input" . NIL) (parse-A-then-B '(NIL)))
      (= '(("A" "B") . ("C")) (parse-A-then-B (chop "ABC")))
      )))

(detest t-or-p ()
  (let (parse-A (pchar2 "A")
        parse-B (pchar2 "B")
        parse-A-else-B (or-p 'parse-A 'parse-B))
    (check
      (= '(("A") . ("Z" "Z")) (parse-A-else-B (chop "AZZ")))
      (= '(("B") . ("Z" "Z")) (parse-A-else-B (chop "BZZ")))
      (= '(NIL "Expecting 'B'. Got 'C'" . ("C" "Z" "Z")) (parse-A-else-B (chop "CZZ")))
      )))

(detest t-and-or ()
  (let (parse-A (pchar2 "A")
        parse-B (pchar2 "B")
        parse-C (pchar2 "C")
        parse-B-else-C (or-p 'parse-B 'parse-C)
        parse-A-then-B-or-C (and-p 'parse-A 'parse-B-else-C))
    (check
      (= '(("A" "B") . ("Z")) (parse-A-then-B-or-C (chop "ABZ")))
      (= '(("A" "C") . ("Z")) (parse-A-then-B-or-C (chop "ACZ")))
      (= '(NIL "Expecting 'A'. Got 'Q'" . ("Q" "B" "Z")) (parse-A-then-B-or-C (chop "QBZ")))
      (= '(NIL "Expecting 'C'. Got 'Q'" . ("Q" "Z")) (parse-A-then-B-or-C (chop "AQZ")))
      )))

(detest t-choice ()
  (let (parse-A (pchar2 "A")
        parse-B (pchar2 "B")
        parse-C (pchar2 "C")
        parse-choice (choice 'parse-A 'parse-B 'parse-C))
    (check
      (= '(("A") . ("Z" "Z")) (parse-choice (chop "AZZ")))
      (= '(("B") . ("Z" "Z")) (parse-choice (chop "BZZ")))
      (= '(("C") . ("Z" "Z")) (parse-choice (chop "CZZ")))
      (= '(NIL "Expecting 'C'. Got 'Q'" . ("Q" "Z" "Z")) (parse-choice (chop "QZZ")))
      )))

(detest t-any-of ()
  (let (parse-any-of (any-of "A" "B" "C"))
    (check
      (= '(("A") . ("Z" "Z")) (parse-any-of (chop "AZZ")))
      (= '(("B") . ("Z" "Z")) (parse-any-of (chop "BZZ")))
      (= '(("C") . ("Z" "Z")) (parse-any-of (chop "CZZ")))
      (= '(NIL "Expecting 'C'. Got 'Q'" . ("Q" "Z" "Z")) (parse-any-of (chop "QZZ")))
      (= '(("a") . ("B" "C")) (parse-lowc (chop "aBC")))
      (= '(NIL "Expecting 'z'. Got 'A'" . ("A" "B" "C")) (parse-lowc (chop "ABC")))
      (= '(("A") . ("B" "C")) (parse-uppc (chop "ABC")))
      (= '(NIL "Expecting 'Z'. Got 'a'" . ("a" "B" "C")) (parse-uppc (chop "aBC")))
      (= '(("1") . ("B" "C")) (parse-digit (chop "1BC")))
      (= '(("9") . ("B" "C")) (parse-digit (chop "9BC")))
      (= '(NIL "Expecting '9'. Got '|'" . ("|" "B" "C")) (parse-digit (chop "|BC")))
      )))

(detest t-all-of ()
  (let (parse-abc (pstring "ABC"))
    (check
      (= '(("A" "B" "C") . ("D" "E")) (parse-abc (chop "ABCDE")))
      )))

(detest t-mapping ()
  (check
    (= '(("1" "2" "3") . ("C")) (parse3digits (chop "123C")))
    (= '(NIL "Expecting '9'. Got 'B'" . ("B" "C")) (parse3digits (chop "12BC")))
    (= '(("123") . ("C")) (parse3digits-as-str (chop "123C")))
    (= '((123) . ("C")) (parse3digits-as-int (chop "123C")))
    (= '(("ABC") . ("D" "E")) ((pstring2 "ABC") (chop "ABCDE")))
    (= '(NIL "Expecting 'B'. Got '|'" . ("|" "C" "D" "E")) ((pstring2 "ABC") (chop "A|CDE")))
    (= '(NIL "Expecting 'C'. Got '|'" . ("|" "D" "E")) ((pstring2 "ABC") (chop "AB|DE")))
    ))

(detest t-many-1 ()
  (let many-a (many (pchar2 "A"))
    (check
      (= '(("A") . ("B" "C" "D")) (many-a (chop "ABCD")))
      (= '(("A" "A") . ("C" "D")) (many-a (chop "AACD")))
      (= '(("A" "A" "A") . ("D")) (many-a (chop "AAAD")))
      (= '(() . ("|" "B" "C" "D")) (many-a (chop "|BCD")))
      )))

(detest t-many-2 ()
  (let many-ab (many (pstring2 "AB"))
    (check
      (= '(("AB") . ("C" "D")) (many-ab (chop "ABCD")))
      (= '(("AB" "AB") . ("C" "D")) (many-ab (chop "ABABCD")))
      (= '(() . ("Z" "C" "D")) (many-ab (chop "ZCD")))
      (= '(() . ("A" "Z" "C" "D")) (many-ab (chop "AZCD")))
      )))

(detest t-many-3 ()
  (let many-ws (many 'parse-ws)
    (check
      (= '(() . ("A" "B" "C")) (many-ws (chop "ABC")))
      (= '((" ") . ("A" "B" "C")) (many-ws (chop " ABC")))
      (= '(("\t") . ("A" "B" "C")) (many-ws (chop "\tABC")))
      (= '(("\n") . ("A" "B" "C")) (many-ws (chop "\nABC")))
      (= '(("\r") . ("A" "B" "C")) (many-ws (chop "\rABC")))
      )))

(detest t-many-4 ()
  (let m-digit+ (many+ 'parse-digit)
    (check
      (= '(("1") . ("A" "B" "C")) (m-digit+ (chop "1ABC")))
      (= '(("1" "2") . ("B" "C")) (m-digit+ (chop "12BC")))
      (= '(("1" "2" "3") . ("C")) (m-digit+ (chop "123C")))
      (= '(("1" "2" "3" "4") . ()) (m-digit+ (chop "1234")))
      (= '(NIL "Expecting '9'. Got 'A'" . ("A" "B" "C")) (m-digit+ (chop "ABC")))
      )))

(detest t-many-5 ()
  (check
    (= '((1) . ("A" "B" "C")) (pint (chop "1ABC")))
    (= '((12) . ("B" "C")) (pint (chop "12BC")))
    (= '((123) . ("C")) (pint (chop "123C")))
    (= '((1234) . ()) (pint (chop "1234")))
    (= '(NIL "Expecting '9'. Got 'A'" . ("A" "B" "C")) (pint (chop "ABC")))
    ))

(detest t-opt ()
  (let digit-then-comma (sequence 'parse-digit (opt (pchar2 ",")))
    (check
      (= '(("1" ",") . NIL) (digit-then-comma (chop "1,")))
      (= '(("1" NIL) . NIL) (digit-then-comma (chop "1")))
      (= '((123) . ("C")) (pint2 (chop "123C")))
      (= '((-123) . ("C")) (pint2 (chop "-123C")))
      )))

(detest t-drop ()
  (let (ws (map-p 'pack (many+ 'parse-ws))
        ab (pstring2 "AB")
        cd (pstring2 "CD")
        parser (and-p (and-p-drop-right 'ab 'ws) 'cd))
    (check
      (= '(("AB" "CD") . ("F")) (parser (chop "AB \tCDF")))
      )))

(detest t-between ()
  (let (pquote (pchar2 "'")
        quoted-int (between pquote pint pquote))
    (check
      (= '((1234) . NIL) (quoted-int (chop "'1234'")))
      (= '(NIL "Expecting '''. Got '1'" . ("1" "2" "3" "4")) (quoted-int (chop "1234")))
      )))

(detest t-sep-by ()
  (let (comma (pchar2 ",")
        digit (sep-by parse-digit comma)
        digit+ (sep-by+ parse-digit comma))
    (check
      (= '(("1") . (";")) (digit+ (chop "1;")))
      (= '(("1" "2") . (";")) (digit+ (chop "1,2;")))
      (= '(("1" "2" "3") . (";")) (digit+ (chop "1,2,3;")))
      (= '(NIL "Expecting '9'. Got 'Z'" . ("Z" ";")) (digit+ (chop "Z;")))
      (= '(("1") . (";")) (digit (chop "1;")))
      (= '(("1" "2") . (";")) (digit (chop "1,2;")))
      (= '(("1" "2" "3") . (";")) (digit (chop "1,2,3;")))
      (= '((NIL) . ("Z" ";")) (digit (chop "Z;")))
      )))

(detest t-parser ()
  (combine-results
    (t-pchar)
    (t-pchar2)
    (t-pchar3)
    (t-and-p)
    (t-or-p)
    (t-and-or)
    (t-choice)
    (t-any-of)
    (t-all-of)
    (t-mapping)
    (t-many-1)
    (t-many-2)
    (t-many-3)
    (t-many-4)
    (t-many-5)
    (t-opt)
    (t-drop)
    (t-between)
    (t-sep-by)
    ))

(t-parser)