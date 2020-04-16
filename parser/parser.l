(setq *Lowcase (make (for I 26 (link (char (+ 96 I))))))
(setq *Uppcase (mapcar uppc *Lowcase))
(setq *Digits (mapcar format (range 0 9)))
(setq *Whitespace (chop " \t\n\r"))

(de pchar (Char Chars)
  (let C (car Chars)
    (cond
      ((= C NIL) (cons NIL (text "No more input") NIL))
      ((= C Char) (cons (list C) (cdr Chars)))
      (T (cons NIL (text "Expecting '@1'. Got '@2'" Char C) Chars)))))

(de pchar2 (@Char)
  (fill '((Chars) (pchar @Char Chars))))

(de and-p (@Parser1 @Parser2)
  (fill '((Chars)
    (let Res1 ('@Parser1 Chars)
      (if (car Res1)
        (let Res2 ('@Parser2 (cdr Res1))
          (if (car Res2)
            (cons (append (car Res1) (car Res2)) (cdr Res2))
            Res2))
        Res1)))))

(de or-p (@Parser1 @Parser2)
  (fill '((Chars)
    (let Res1 ('@Parser1 Chars)
      (if (car Res1)
        Res1
        ('@Parser2 Chars))))))

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(de choice @
  (reduce 'or-p (rest)))

(de any-of @
  (apply choice (mapcar pchar2 (rest))))

(def 'parse-lowc (apply any-of *Lowcase))

(def 'parse-uppc (apply any-of *Uppcase))

(def 'parse-digit (apply any-of *Digits))

(def 'parse-ws (apply any-of *Whitespace))

(de sequence @
  (reduce 'and-p (rest)))

(de all-of @
  (apply sequence (mapcar pchar2 (rest))))

(de pstring (String)
  (apply all-of (chop String)))

(def 'parse3digits (sequence 'parse-digit 'parse-digit 'parse-digit))

(de map-p (@Fun @Parser)
  (fill '((Chars)
    (let Res ('@Parser Chars)
      (if (car Res)
        (cons (list ('@Fun (car Res))) (cdr Res))
        Res)))))

(de apply-p (@Fun @Parser)
  (fill '((Chars)
    (let Res ('@Parser Chars)
      (if (car Res)
        (cons (list (apply '@Fun (car Res))) (cdr Res))
        Res)))))

(de pstring2 (String)
  (map-p 'pack (pstring String)))

(def 'parse3digits-as-str (map-p 'pack 'parse3digits))

(def 'parse3digits-as-int (map-p 'format 'parse3digits-as-str))

(de return-p (@X)
  (fill '((Chars)
    (cons (list @X) Chars))))

(de parse-zero-or-more (Parser Chars)
  (let ((Val . RestChars) (Parser Chars))
    (ifn Val
      (cons '() Chars)
      (let ((Val2 . RestChars2) (parse-zero-or-more Parser RestChars))
        (cons (append Val Val2) RestChars2)))))

(de many (@Parser)
  (fill '((Chars)
    (parse-zero-or-more '@Parser Chars))))

(de many+ (@Parser)
  (fill '((Chars)
    (let Res ('@Parser Chars)
      (ifn (car Res)
        Res
        (parse-zero-or-more '@Parser Chars))))))

(def 'pint (map-p 'format (map-p 'pack (many+ 'parse-digit))))

(de opt (@Parser)
  (fill '((Chars)
    (let Res ('@Parser Chars)
      (if (car Res)
        Res
        ((return-p NIL) Chars))))))

(def 'pint2 (map-p 'format (map-p 'pack (sequence (opt (pchar2 "-")) (many+ 'parse-digit)))))

(de and-p-drop-left (P1 P2)
  (map-p 'cadr (and-p P1 P2)))

(de and-p-drop-right (P1 P2)
  (map-p 'car (and-p P1 P2)))

(de between (P1 P2 P3)
  (and-p-drop-right (and-p-drop-left P1 P2) P3))

(de sep-by+ (P PSep)
  (and-p P (many (and-p-drop-left PSep P))))

(de sep-by (P PSep)
  (or-p (sep-by+ P PSep) (return-p NIL)))

(de bind-p (@Fun @P)
  (fill '((Chars)
    (let Res ('@P Chars)
      (if (car Res)
        (let P2 ('@Fun (car Res))
          (P2 (cdr Res)))
        Res)))))
