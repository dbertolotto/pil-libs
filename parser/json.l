(load "parser.l")

### JSON

(def 'jnull
  (labelp "JSON null"
    (mapp 'any (stringp "null"))))

(def 'jtrue
  (labelp "JSON true"
    (mapp 'any (stringp "true"))))

(def 'jfalse
  (labelp "JSON false"
    (mapp 'any (stringp "false"))))

(def 'junesc
  (satp '((X) (not (or (= X "\\") (= X "\""))))))

(setq *Jesc
  (list
    '("\\\"" . "\"")
    '("\\\\" . "\\")
    '("\\/" . "/")
    '("\\b" . `(uni "232B"))
    '("\\f" . `(uni "240A"))
    '("\\n" . "\n")
    '("\\r" . "\r")
    '("\\t" . "\t")
    ))

(def 'jesc
  (apply 'plusp (mapcar '((X) (ignorep (stringp (car X)) (cdr X))) *Jesc)))

(def 'juni
  (let (bl (charp "\\")
        uc (charp "u")
        hx (seqp hexp hexp hexp hexp)
        code (mapp 'uni (mapp 'pack hx)))
    (droplp bl (droplp uc code))))

(def 'jchar
  (plusp 'junesc 'jesc 'juni))

(de handle-empty-string (S)
  (if S S 'nils))

(def 'jstring
  (let (quotep (charp "\"")
        manych (mapp 'pack (manyp 'jchar)))
    (labelp "JSON string"
      (mapp 'handle-empty-string
            (betweenp quotep manych quotep)))))

(de to-num (Sign Int Frac SignExp Exp)
  (let (Expn (format Exp)
        ExpVal (case Expn
                 (NIL 1)
                 (0 1)
                 (T (apply * (need Expn 10))))
        ExpScl (if (= SignExp "-") Expn 0)
        FracScl (length Frac))
    (scl (max *Scl (+ FracScl ExpScl)))
    (let (Num (format (pack Sign Int "." Frac) *Scl)
          op (if (= SignExp "-") '/ '*))
      (op Num ExpVal))))

(def 'jdigit19
  (satp '((X) (and (>= X "1") (<= X "9")))))

(def 'jsign
  (plusp (charp "+") (charp "-") 'nilp))

(def 'jint
  (seqp 'jsign
        (mapp 'pack (plusp (charp "0")
                           (seqp 'jdigit19 (manyp digitp))))))

(def 'jfrac
  (droplp (charp ".")
          (mapp 'pack (many1p digitp))))

(def 'jexp
  (droplp (plusp (charp "e") (charp "E"))
          (seqp 'jsign
                (mapp 'pack (many1p digitp)))))

(def 'jnum
  (seqp 'jint (optp 'jfrac) (optp 'jexp)))

(def 'jnumber
  (labelp "JSON number"
    (mapp '((X) (apply 'to-num X))
          (mapp '((X) (fish atom X)) 'jnum))))

(de to-array (Lst)
  (append '(T) Lst))

(def 'jarray
  (let (left (droprp (charp "[") 'spacesp)
        right (droprp (charp "]") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        value (droprp 'jvalue 'spacesp)
        values (sepbyp value comma))
    (labelp "JSON array"
      (mapp 'to-array (betweenp left values right)))))

(def 'jobject
  (let (left (droprp (charp "{") 'spacesp)
        right (droprp (charp "}") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        colon (droprp (charp ":") 'spacesp)
        key (droprp 'jstring 'spacesp)
        value (droprp 'jvalue 'spacesp)
        keyval (seqp (droprp key colon) value)
        keyvals (sepbyp keyval comma))
    (labelp "JSON object"
      (betweenp left keyvals right))))

(def 'jvalue
  (plusp 'jobject 'jarray 'jstring 'jnumber 'jtrue 'jfalse 'jnull))

(de read-file (Path)
  (in Path (till NIL T)))

(setq ex01 (read-file "ex01.json"))
(setq ex02 (read-file "ex02.json"))
(setq ex03 (read-file "ex03.json"))
(setq ex04 (read-file "ex04.json"))
(setq ex05 (read-file "ex05.json"))
(setq test02 (read-file "test02.json"))

