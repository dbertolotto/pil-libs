(load+ "#parser/json.l" "#lib/unit.l" "#lib/io.l" "#lib/crypto.l")
(symbols 'pico 'unit 'json 'parser 'io 'crypto)

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

(de to-str (Jval)
  (pipe (print-json Jval) (till NIL T)))

# sample files
(setq ex01 (read-file "ex01.json"))
(setq ex02 (read-file "ex02.json"))
(setq ex03 (read-file "ex03.json"))
(setq ex04 (read-file "ex04.json"))
(setq ex05 (read-file "ex05.json"))
(setq test02 (read-file "test02.json"))

### tests

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
    ('nils (parsev jstring "\"\""))
    ("a" (parsev jstring "\"a\""))
    ("ab" (parsev jstring "\"ab\""))
    ("ab\tde" (parsev jstring "\"ab\\tde\""))
    ("ab?de" (parsev jstring "\"ab\\u003fde\""))
    ))

(deftest t-jnumber ()
  (let scl0= '((A B) (scl 0) (= A B))
    (bind '((*Scl . 0))
      (check 'scl0=
        (1 (to-num "" "1" "" "" ""))
        (1 (to-num "+" "1" "" "" ""))
        (-1 (to-num "-" "1" "" "" ""))
        (-12 (to-num "-" "1" "2" "" ""))
        (-1200 (to-num "-" "1" "2" "" "2"))
        (12 (to-num "" "1" "2" "-" "2"))
        (0 (parsev jnumber "0"))
        (0 (parsev jnumber "-0"))
        (34 (parsev jnumber "34"))
        (-34 (parsev jnumber "-34"))
        (345 (parsev jnumber "34.5"))
        (-345 (parsev jnumber "-34.5"))
        (12 (parsev jnumber "+1.2e-3"))
        (1200 (parsev jnumber "+1.2e2"))
        (12 (parsev jnumber "+1.2e0"))
        (12 (parsev jnumber "+1.2e-0"))
        (-12 (parsev jnumber "-1.2e-3"))
        ))))

(deftest t-jarray ()
  (check=
    ('(T) (parsev jarray "[ ]"))
    ('(T) (parsev jarray "[\n]"))
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
    ('() (parsev jobject "{\n}"))
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
     (parsev jvalue "[\"a\",{\"b\":false}]"))
    ('(("a" (T false)))
     (parsev jvalue "{\"a\":[false]}"))
    ('(("a" (T false 1)))
     (parsev jvalue "{\"a\":[false,1]}"))
    ('(("a" (T false (("b" 1)))))
     (parsev jvalue "{\"a\":[false,{\"b\":1}]}"))
    ))

(deftest t-print ()
  (check=
    ("\"\"" (to-str 'nils))
    ("[]" (to-str '(T)))
    ("[\"a\"]" (to-str '(T "a")))
    ("[\"a\",\"b\"]" (to-str '(T "a" "b")))
    ("[1,2]" (to-str '(T 1 2)))
    ("{}" (to-str '()))
    ("{\"a\":false}" (to-str '(("a" false))))
    ("{\"a\":false,\"b\":12}"
     (to-str '(("a" false) ("b" 12))))
    ))

(deftest t-read-print ()
  (let (md5= '((A B) (= (md5 A) (md5 B)))
        *Json-pretty-print T)
    (check 'md5=
      (ex01 (pack (to-str (parsev jvalue ex01)) "\n"))
      (ex02 (pack (to-str (parsev jvalue ex02)) "\n"))
      (ex03 (pack (to-str (parsev jvalue ex03)) "\n"))
      (ex04 (pack (to-str (parsev jvalue ex04)) "\n"))
      (ex05 (pack (to-str (parsev jvalue ex05)) "\n"))
      (test02 (pack (to-str (parsev jvalue test02)) "\n"))
      )))

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
    (t-print)
    (t-read-print)
    ))

(t-json)

#(mapcar '((x y) (when (println x y)))
#        (chop ex04)
#        (append (chop (to-str (parsev jvalue ex04))) "\n"))
#(out "ex04.json.out" (prinl (to-str (parsev jvalue ex04))))
