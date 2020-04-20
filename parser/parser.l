(setq *Lowcase (make (for I 26 (link (char (+ 96 I))))))
(setq *Uppcase (mapcar uppc *Lowcase))
(setq *Digits (mapcar format (range 0 9)))
(setq *Whitespace (chop " \t\n\r"))

(class +PSuc)
(dm T (Val RestInp)
  (=: Val Val)
  (=: RestInp RestInp))
(dm str> ()
  (text "@1" (: Val)))

(class +PFail)
(dm T (Label Err)
  (=: Label Label)
  (=: Err Err))
(dm str> ()
  (text "Error parsing @1\n@2" (: Label) (: Err)))

(de satisfy (@Pred @Label)
  (fill '((Inp)
          (if (= Inp NIL)
            (new '(+PFail) @Label "No more input")
            (let First (car Inp)
              (if ('@Pred First)
                (new '(+PSuc) First (cdr Inp))
                (new '(+PFail) @Label (text "Unexpected '@1'" First))))))))

(de pchar3 (@Char)
  (satisfy (fill '((First) (= First @Char))) (text "@1" @Char)))

(def 'digit (satisfy '((X) (member X *Digits)) "digit"))
(def 'ws (satisfy '((X) (member X *Whitespace)) "whitespace"))

(de pchar (Char Chars)
  (let C (car Chars)
    (cond
      ((= C NIL) (cons NIL (text "No more input") NIL))
      ((= C Char) (cons (list C) (cdr Chars)))
      (T (cons NIL (text "Expecting '@1'. Got '@2'" Char C) Chars)))))

(de pchar2 (@Char)
  (fill '((Chars) (pchar @Char Chars))))

(de and-p (@Parser1 @Parser2)
  (fill
    '((Chars)
      (let Res1 ('@Parser1 Chars)
        (if (car Res1)
          (let Res2 ('@Parser2 (cdr Res1))
            (if (car Res2)
              (cons (append (car Res1) (car Res2)) (cdr Res2))
              Res2))
          Res1)))))

(de and-p2 (@P1 @P2)
  (fill
    '((Chars)
      (let Res1 ('@P1 Chars)
        (if (isa '+PSuc Res1)
          (let Res2 ('@P2 (get Res1 'RestInp))
            (if (isa '+PSuc Res2)
              (let (V1 (get Res1 'Val)
                    V2 (get Res2 'Val))
                (new '(+PSuc)
                     (append (if (atom V1) (list V1) V1)
                             (if (atom V2) (list V2) V2))
                     (get Res2 'RestInp)))
              Res2))
          Res1)))))

(de or-p (@Parser1 @Parser2)
 (fill
   '((Chars)
     (let Res1 ('@Parser1 Chars)
       (if (car Res1)
         Res1
         ('@Parser2 Chars))))))

(de or-p2 (@P1 @P2)
 (fill
   '((Chars)
     (let Res1 ('@P1 Chars)
       (if (isa '+PSuc Res1)
         Res1
         ('@P2 Chars))))))

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

(de choice2 @
  (reduce 'or-p2 (rest)))

(de any-of2 @
  (apply choice2 (mapcar pchar3 (rest))))

(def 'parse-lowc2 (apply any-of2 *Lowcase))
(def 'parse-uppc2 (apply any-of2 *Uppcase))
(def 'parse-digit2 (apply any-of2 *Digits))
(def 'parse-ws2 (apply any-of2 *Whitespace))

(de sequence @
  (reduce 'and-p (rest)))

(de all-of @
  (apply sequence (mapcar pchar2 (rest))))

(de pstring (String)
  (apply all-of (chop String)))

(def 'parse3digits (sequence 'parse-digit 'parse-digit 'parse-digit))

(de sequence2 @
  (reduce 'and-p2 (rest)))

(de all-of2 @
  (apply sequence2 (mapcar pchar3 (rest))))

(de pstring3 (String)
  (apply all-of2 (chop String)))

(def 'parse3digits2 (sequence2 'parse-digit2 'parse-digit2 'parse-digit2))

(de map-p (@Fun @Parser)
  (fill
    '((Chars)
      (let Res ('@Parser Chars)
        (if (car Res)
          (cons (list ('@Fun (car Res))) (cdr Res))
          Res)))))

(def 'parse3digits-as-str (map-p 'pack 'parse3digits))
(def 'parse3digits-as-int (map-p 'format 'parse3digits-as-str))

(de pstring2 (String)
  (map-p 'pack (pstring String)))

(de map-p2 (@F @P)
  (fill
    '((Chars)
      (let Res ('@P Chars)
        (if (isa '+PSuc Res)
          (new '(+PSuc) ('@F (get Res 'Val)) (get Res 'RestInp))
          Res)))))

(def 'parse3digits-as-str2 (map-p2 'pack 'parse3digits2))
(def 'parse3digits-as-int2 (map-p2 'format 'parse3digits-as-str2))

(de pstring4 (String)
  (map-p2 'pack (pstring3 String)))

(de return-p (@X)
  (fill
    '((Chars)
      (cons (list @X) Chars))))

(de return-p2 (@X)
  (fill
    '((Chars)
      (new '(+PSuc) @X Chars))))

(de parse-zero-or-more (Parser Chars)
  (let ((Val . RestChars) (Parser Chars))
    (ifn Val
         (cons '() Chars)
         (let ((Val2 . RestChars2) (parse-zero-or-more Parser RestChars))
           (cons (append Val Val2) RestChars2)))))

(de many (@Parser)
  (fill
    '((Chars)
      (parse-zero-or-more '@Parser Chars))))

(de many+ (@Parser)
  (fill
    '((Chars)
      (let Res ('@Parser Chars)
        (ifn (car Res)
             Res
             (parse-zero-or-more '@Parser Chars))))))

(def 'pint (map-p 'format (map-p 'pack (many+ 'parse-digit))))

(de parse-zero-or-more2 (P Chars)
  (let Res1 (P Chars)
    (if (isa '+PFail Res1)
      (new '(+PSuc) '() Chars)
      (let (Res2 (parse-zero-or-more2 P (get Res1 'RestInp))
            V1 (get Res1 'Val)
            V2 (get Res2 'Val))
        (new '(+PSuc)
             (append (if (and V1 (atom V1)) (list V1) V1)
                     (if (and V2 (atom V2)) (list V2) V2))
             (get Res2 'RestInp))))))

(de many2 (@P)
  (fill
    '((Chars)
      (parse-zero-or-more2 '@P Chars))))

(de many2+ (@P)
  (fill
    '((Chars)
      (let Res1 ('@P Chars)
        (if (isa '+PFail Res)
          Res
          (let (Res2 (parse-zero-or-more2 P (get Res1 'RestInp))
                V1 (get Res1 'Val)
                V2 (get Res2 'Val))
            (new '(+PSuc)
                 (append (if (and V1 (atom V1)) (list V1) V1)
                         (if (and V2 (atom V2)) (list V2) V2))
                 (get Res2 'RestInp))))))))

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
