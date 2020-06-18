(load "json.l" "../lib/unit.l")
(symbols 'pico 'unit 'json 'parser)

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

(de uni (S)
  (char (hex S)))

(deftest t-jnull ()
  (check=
    ('null (parsev jnull "null"))
    ((err-help 1 3 "jnull" "r")
     (parsem jnull "nurl"))
    ))

(deftest t-jtrue ()
  (check=
    ('true (parsev jtrue "true"))
    ((err-help 1 3 "jtrue" "r")
     (parsem jtrue "trre"))
    ))

(deftest t-jfalse ()
  (check=
    ('false (parsev jfalse "false"))
    ((err-help 1 3 "jfalse" "r")
     (parsem jfalse "farse"))
    ))

(deftest t-jbool ()
  (check=
    ('true (parsev jbool "true"))
    ((err-help 1 3 "jbool" "r")
     (parsem jbool "trre"))
    ('false (parsev jbool "false"))
    ))

(deftest t-junescaped ()
  (let mju (manyp junescaped)
    (check=
      ("a" (parsev junescaped "a"))
      (T (perr? (parse junescaped "\\")))
      ('() (parsev mju "\\"))
      ('("a") (parsev mju "a"))
      ('("a" "b") (parsev mju "ab"))
      )))

(deftest t-jescaped ()
  (check=
    ("\\" (parsev jescaped "\\\\"))
    ("\t" (parsev jescaped "\\t"))
    ))

(deftest t-junicode ()
  (check=
    ((uni "263A") (parsev junicode "\\u263A"))
    ))

(deftest t-jchar ()
  (let (ajc (andp jchar jchar)
        sjc (sequencep jchar jchar)
        mjc (manyp jchar))
    (check=
      ("a" (parsev jchar "a"))
      ("a" (parsev jchar "ab"))
      ("\\" (parsev jchar "\\\\"))
      ((uni "263A") (parsev jchar "\\u263A"))
      ('("a" "\\") (parsev ajc "a\\\\"))
      ('("a" "b") (parsev ajc "ab"))
      ('("a" "\\") (parsev sjc "a\\\\"))
      ('("a" "b") (parsev sjc "ab"))
      ('("\t") (parsev mjc "\\t"))
      ('("a" "b") (parsev mjc "ab"))
      ('("a" "b" `(uni "263A")) (parsev mjc "ab\\u263A"))
    )))

(deftest t-jstring ()
  (check=
    ("" (parsev jstring "\"\""))
    ("a" (parsev jstring "\"a\""))
    ("ab" (parsev jstring "\"ab\""))
    ("ab\tde" (parsev jstring "\"ab\\tde\""))
    ("ab?de" (parsev jstring "\"ab\\u003fde\""))
    ))

(deftest t-jnumber ()
  (let scl0= '((A B) (scl 0) (= A B))
    (check 'scl0=
      (1 (to-num "" "1" "" "" ""))
      (1 (to-num "+" "1" "" "" ""))
      (-1 (to-num "-" "1" "" "" ""))
      (-12 (to-num "-" "1" "2" "" ""))
      (-1200 (to-num "-" "1" "2" "" "2"))
      (12 (to-num "" "1" "2" "-" "2"))
      (0 (parsev jnumber "0"))
      (12 (parsev jnumber "+1.2e-3"))
      (0 (scl 0)) # reset scl
      )))

(deftest t-jarray ()
  (check=
    ('(T) (parsev jarray "[ ]"))
    ('(T 1) (parsev jarray "[1]"))
    ('(T "a" false 12)
     (parsev jarray "[ \"a\" , false , 12 ]"))
    ('(T "ab" (T true 12) "cd")
     (parsev jarray "[\"ab\",[true,12],\"cd\"]"))
    (T (perr? (parse jarray "[\"a\" \"b\"]")))
    ((err-help 1 6 "jarray" "\"")
     (parsem jarray "[\"a\" \"b\"]"))
    ((err-help 2 5 "jarray" "\"")
     (parsem jarray "[\"a\",\n\"b\" \"c\"]"))
    ((err-help 1 6 "jarray" "2")
     (parsem jarray "[\"a\",[\"c\"23],23]"))
    ))

(deftest t-jobject ()
  (check=
    ('() (parsev jobject "{ }"))
    ('(("a" false))
     (parsev jobject "{\"a\":false}"))
    ('(("b" null))
     (parsev jobject "{ \"b\" : null }"))
    ('(("b" null) ("c" 12))
     (parsev jobject "{\"b\":null , \"c\":12}"))
    (T (perr? (parse jobject "{\"b\":null \"c\":12}")))
    ((err-help 1 11 "jobject" "\"")
     (parsem jobject "{\"b\":null \"c\":12}"))
    ((err-help 1 11 "jobject" "\"")
     (parsem jobject "{\"a\":{\"b\":null \"c\":12}}"))
    ))

(deftest t-jvalue ()
  (check=
    ('(T "a" (("b" false)))
     (parsev jarray "[\"a\",{\"b\":false}]"))
    ('(("a" (T false)))
     (parsev jobject "{\"a\":[false]}"))
    ('(("a" (T false 1)))
     (parsev jobject "{\"a\":[false,1]}"))
    ('(("a" (T false (("b" 1)))))
     (parsev jobject "{\"a\":[false,{\"b\":1}]}"))
    ))

(deftest t-json ()
  (combine-results
    (t-jnull)
    (t-jtrue)
    (t-jfalse)
    (t-jbool)
    (t-junescaped)
    (t-jescaped)
    (t-junicode)
    (t-jchar)
    (t-jstring)
    (t-jnumber)
    (t-jarray)
    (t-jobject)
    (t-jvalue)
    ))

(t-json)

### json

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
