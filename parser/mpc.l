
(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

### State

(de updates (@F)
  (fill '(State)
    (list State ('@F State))))

(de sets (@S)
  (macro
    (updates '((S) '@S))))

(def 'fetchs
  (updates '((S) S)))

### Parser

(de resultp (@V)
  (fill '((Inp)
    (list (list '@V Inp)))))

(def 'zerop
  (fill '((Inp)
    '())))

(def 'itemp
  (fill '((Inp)
    (if Inp
      (list (list (car Inp) (cdr Inp)))
      '()))))

(de bindp (@P @F)
  (fill '((Inp)
    #(println Inp)
    (let R ('@P Inp)
      (if R
        (let (V (caar R)
              Rest (cadar R))
          (('@F V) Rest))
        R)))))

(de bind2p (@P1 @P2 @F)
  (eval
    (fill
      '(bindp '@P1
              '((V1)
                (let (@V1 V1
                      F2 (fill '((V2) (resultp ('@F '@V1 V2)))))
                  (bindp '@P2 F2))))
      '(@P1 @P2 @F))))

(de seq2p (P1 P2)
  (bind2p P1 P2 'cons))

(de seqp @
  (reduce 'seq2p (rest)))

(de satp (@Pred)
  (macro
    (bindp 'itemp '((X) (if ('@Pred X) (resultp X) 'zerop)))))

(de mapp (@Fn @P)
  (macro
    (bindp '@P '((V) (resultp ('@Fn V))))))

(de charp (@C)
  (macro
    (satp '((X) (= @C X)))))

(def 'digitp
  (satp '((X) (and (>= X "0") (<= X "9")))))

(def 'lowerp
  (satp '((X) (and (>= X "a") (<= X "z")))))

(def 'upperp
  (satp '((X) (and (>= X "A") (<= X "Z")))))

(def 'hexletterp
  (satp '((X) (and (>= X "A") (<= X "F")))))

(de plus2p (@P1 @P2)
  (fill '((Inp)
    (let (R1 ('@P1 Inp)
          R2 ('@P2 Inp))
      (append R1 R2)))))

(de plusp @
  (reduce 'plus2p (rest)))

(de orp @Plist
  (fill '((Inp)
    (let Car (eval (car '@Plist))
      (if Car
        (let (R (Car Inp)
              Cdr (cdr '@Plist))
          (if (and (not R) Cdr)
            ((eval (cons 'orp Cdr)) Inp)
            R))
        (zerop Inp))))))

(def 'letterp
  (plusp 'lowerp 'upperp))

(def 'alphanump
  (plusp 'letterp 'digitp))

(def 'hexp
  (plusp 'digitp 'hexletterp))

(def 'wordp
  (plusp (mapp 'pack (seqp 'letterp 'wordp)) (resultp "")))

(de stringp (String)
  (let Stream (if (atom String) (chop String) String)
    (if Stream
      (mapp 'pack (seqp (charp (car Stream)) (stringp (cdr Stream))))
      (resultp ""))))

(de manyp (@P)
  (fill '((Inp)
    ((plusp (seqp '@P (manyp '@P)) (resultp '())) Inp))))

# alt version of wordp with many
(def 'wordp2
  (mapp 'pack (manyp 'letterp)))

(def 'identp
  (seqp 'lowerp (manyp 'alphanump)))

(de many1p (@P)
  (fill '((Inp)
    ((seqp '@P (manyp '@P)) Inp))))

# alt version
(de many1p (P)
  (seqp P (manyp P)))

(def 'natp
  (bindp (many1p 'digitp) '((X) (resultp (format X)))))

(def 'intp
  (bindp (seqp (plusp (charp "-") (charp "+") (resultp "")) (many1p 'digitp))
         '((V) (resultp (format (pack V))))))

(def 'spacep
  (satp '((X) (member X '(" " "\t" "\n" "\r")))))

(def 'spacesp
  (manyp 'spacep))

(de droplp (P1 P2)
  (bind2p P1 P2 '((V1 V2) V2)))

(de droprp (P1 P2)
  (bind2p P1 P2 '((V1 V2) V1)))

(de sepby1p (P Sep)
  (seqp P (manyp (droplp Sep P))))

(de betweenp (Open P Close)
  (droplp Open (droprp P Close)))

(de sepbyp (P Sep)
  (plusp (sepby1p P Sep) (resultp '())))

(def 'intsp
  (betweenp (charp "[") (sepby1p 'intp (charp ",")) (charp "]")))

(de ignorep (@P @W)
  (macro
    (bindp '@P '((V) (resultp '@W)))))

(de optp (P)
  (plusp P (resultp '())))

### JSON

(def 'jnull
  (mapp 'any (stringp "null")))

(def 'jtrue
  (mapp 'any (stringp "true")))

(def 'jfalse
  (mapp 'any (stringp "false")))

(def 'junesc
  (satp '((X) (not (or (= X "\\") (= X "\""))))))

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

(def 'jesc
  (apply 'plusp (mapcar '((X) (ignorep (stringp (car X)) (cdr X))) *Jesc)))

(def 'juni
  (let (bl (charp "\\")
        uc (charp "u")
        hx (seqp hexp hexp hexp hexp)
        code (mapp 'char (mapp 'hex (mapp 'pack hx))))
    (droplp bl (droplp uc code))))

(def 'jchar
  (plusp 'junesc 'jesc 'juni))

(de handle-empty-string (S)
  (if S S 'nils))

(def 'jstring
  (let (quotep (charp "\"")
        manych (mapp 'pack (manyp 'jchar)))
    (mapp 'handle-empty-string
          (betweenp quotep manych quotep))))

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
  (plusp (charp "+") (charp "-") (resultp "")))

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
  (mapp '((X) (apply 'to-num X))
        (mapp '((X) (fish atom X)) 'jnum)))

(de to-array (Lst)
  (append '(T) Lst))

(def 'jarray
  (let (left (droprp (charp "[") 'spacesp)
        right (droprp (charp "]") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        value (droprp 'jvalue 'spacesp)
        values (sepbyp value comma))
    (mapp 'to-array (betweenp left values right))))

(def 'jobject
  (let (left (droprp (charp "{") 'spacesp)
        right (droprp (charp "}") 'spacesp)
        comma (droprp (charp ",") 'spacesp)
        colon (droprp (charp ":") 'spacesp)
        key (droprp 'jstring 'spacesp)
        value (droprp 'jvalue 'spacesp)
        keyval (seqp (droprp key colon) value)
        keyvals (sepbyp keyval comma))
    (betweenp left keyvals right)))

(def 'jvalue
  (plusp 'jobject 'jarray 'jstring 'jnumber 'jtrue 'jfalse 'jnull))

(de parse (P String)
  ((droplp 'spacesp P) (chop String)))

(de read-file (Path)
  (in Path (till NIL T)))

(setq ex01 (read-file "ex01.json"))
(setq ex02 (read-file "ex02.json"))
(setq ex03 (read-file "ex03.json"))
(setq ex04 (read-file "ex04.json"))
(setq ex05 (read-file "ex05.json"))
(setq test02 (read-file "test02.json"))
