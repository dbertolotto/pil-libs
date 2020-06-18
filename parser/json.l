# json parser
(load "parser.l")
(symbols 'json 'pico 'parser)

(def 'jnull
  (label "jnull"
    (fp 'any (stringp "null"))))

(def 'jtrue
  (label "jtrue"
    (fp 'any (stringp "true"))))

(def 'jfalse
  (label "jfalse"
    (fp 'any (stringp "false"))))

(def 'jbool
  (label "jbool"
    (orp jtrue jfalse)))

(def 'junescaped
  (satisfy '((X) (not (or (= X "\\") (= X "\""))))))

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

(def 'junicode
  (let (bl (charp "\\")
        uc (charp "u")
        hx (sequencep hexp hexp hexp hexp)
        code (fp 'char (fp 'hex (fp 'pack hx))))
    (label "junicode"
      (droplp bl (droplp uc code)))))

(def 'jchar
  (label "jchar"
    (choicep junescaped jescaped junicode)))

(def 'jstring
  (let (quotep (charp "\"")
        manych (fp 'pack (manyp jchar)))
    (label "jstring"
      (droprp (droplp quotep manych) quotep))))

(de to-num (s int fr se ex)
  (let (exn (format ex)
        exval (if exn (apply * (need exn 10)) 1)
        exlen (if (= se "-") exn 0)
        frlen (length fr))
    (setq *Scl (max *Scl (+ frlen exlen)))
    (let (num (format (pack s int "." fr) *Scl)
          op (if (= se "-") '/ '*))
      (op num exval))))

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
    (label "jnumber"
      (applyp 'to-num (mapp 'pack tuple)))))

(def 'spacesp (manyp wsp))

(de to-array (Lst)
  (append '(T) Lst))

(def 'jarray
  (let (left (droprp (charp "[") 'spacesp)
        right (droprp (charp "]") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        value (droprp 'jvalue 'spacesp)
        values (sepbyp value comma))
    (label "jarray"
      (fp 'to-array (betweenp left values right)))))

(def 'jobject
  (let (left (droprp (charp "{") 'spacesp)
        right (droprp (charp "}") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        colon (droprp (charp ":") 'spacesp)
        key (droprp 'jstring 'spacesp)
        value (droprp 'jvalue 'spacesp)
        keyval (andp (droprp key colon) value)
        keyvals (sepbyp keyval comma))
    (label "jobject"
      (betweenp left keyvals right))))

(def 'jvalue
  (label "jvalue"
    (choicep
      'jobject
      'jarray
      'jstring
      'jnumber
      'jbool
      'jnull)))

### print

(de to-chars (File)
  (chop (in File (till NIL T))))

(de to-pinp (File)
  (new '(+PInp) (to-chars File) (pos0)))

(de read-json (Source)
  (cond
    ((isa '+PInp Source) (parse> jvalue Source))
    ((info Source) (parse> jvalue (to-pinp Source)))
    ((str? Source) (parse> jvalue (new '(+PInp) (chop Source) (pos0))))
    ((lst? Source) (parse> jvalue (new '(+PInp) Source (pos0))))
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

