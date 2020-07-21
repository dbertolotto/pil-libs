(setq *ParserInput NIL)

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(de join (Lst Sep)
  (make
    (for (L Lst L)
      (link (pop 'L))
      (NIL L)
      (link Sep))))

(de uni (Str)
  (char (hex Str)))

### State

(class +State)
(dm T (Pos)
  (=: P (if Pos Pos (cons 1 1))))   # Position
(dm row> (Pos)
  (car (nth *ParserInput (cond
                           ((not Pos) (car (: P)))
                           ((num? Pos) Pos)
                           (T (car Pos))))))
(dm car> ()
  (when (row> This)
    (if (car (nth @ (cdr (: P)))) @ "\n")))
(dm pos> ()
  (: P))
(dm prin> ()
  (println (: P) (pack (row> This))))
(dm next> ()
  (let (Car (car> This)
        (R . C) (: P)
        Pos (if (or (= Car "\n") (= Car "\r"))
              (cons (inc R) 1)
              (cons R (inc C))))
    (newstate Pos)))
(dm prev> ()
  (let ((R . C) (: P)
        Pos (if (= C 1)
              (cons (dec R) (inc (length (row> This (dec R)))))
              (cons R (dec C))))
    (newstate Pos)))
(dm copy> ()
  (newstate (: P)))

(de newstate (Pos)
  (new '(+State) Pos))

(class +Success)
(dm T (Val State)
  (=: V Val)
  (=: S State))
(dm prin> ()
  (println (: V))
  (prin> (: S)))
(dm pp> ()
  (println (: V)))

(de success (Val State)
  (new '(+Success) Val State))

(class +Error)
(dm T (Msg State Exp)
  (=: M Msg)
  (=: S State)
  (=: E Exp))
(dm upd-exp> (Exp)
  (=: E (cons Exp (: E)))
  This)
(dm prin> ()
  (prinl (: M) "\n"
         (pack (join (: E) " >> ")))
  (prin> (: S)))
(dm pp> ()
  (prinl "\nError while parsing "
         (pack (join (: E) " >> ")))
  (let (State (: S)
        Pos (pos> State)
        Spc (need (dec (cdr Pos)) " "))
    (prinl (row> State Pos))
    (prin Spc (text "\^-- Row: @1, Col: @2 - " (car Pos) (cdr Pos))))
  (prinl (: M)))

(de error (Msg State Exp)
  (new '(+Error) Msg (prev> State) Exp))

(de error? (Result)
  (when (isa '(+Error) Result) T))

### Parser

(de resultp (@V)
  (fill '((State)
    (list (success '@V State)))))

(de zerop (@Msg)
  (fill '((State)
    (list (error '@Msg State)))))

(de labelp (@Label @P)
  (fill '((State)
    (let (R ('@P State)
          R0 (car R))
      (if (error? R0)
        (mapcar '((X) (upd-exp> X @Label)) R)
        R)))))

(def 'itemp
  (fill '((State)
    (if (car> State)
      (list (success (car> State) (next> State)))
      (list (error "No more input" State))))))

(de bindp (@P @F)
  (fill '((State)
    #(prin> State)
    (let (R ('@P State)
          R0 (car R))
      (ifn (error? R0)
        (let (V (get R0 'V)
              S (get R0 'S))
          (('@F V) S))
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
    (bindp 'itemp
           '((X)
             (cond
               ((= X NIL) (zerop "No more input"))
               (('@Pred X) (resultp X))
               (T (zerop (text "Unexpected '@1'" X))))))))

(de mapp (@Fn @P)
  (macro
    (bindp '@P '((V) (resultp ('@Fn V))))))

(de charp (@C)
  (macro
    (labelp (text "char '@1'" @C)
      (satp '((X) (= @C X))))))

(def 'digitp
  (labelp "digit"
    (satp '((X) (and (>= X "0") (<= X "9"))))))

(def 'lowerp
  (satp '((X) (and (>= X "a") (<= X "z")))))

(def 'upperp
  (satp '((X) (and (>= X "A") (<= X "Z")))))

(def 'hexlowerp
  (satp '((X) (and (>= X "a") (<= X "f")))))

(def 'hexupperp
  (satp '((X) (and (>= X "A") (<= X "F")))))

(de plus2p (@P1 @P2)
  (fill '((State)
    (let (R1 ('@P1 State)
          R10 (car R1))
      (if (error? R10)
        (let (R2 ('@P2 State)
              R20 (car R2))
          (if (error? R20)
            (append R2 R1)
            R2))
        R1)))))

(de plusp @
  (reduce 'plus2p (rest)))

(def 'letterp
  (plusp 'lowerp 'upperp))

(def 'alphanump
  (labelp "alphanumeric"
    (plusp 'letterp 'digitp)))

(def 'hexletterp
  (plusp 'hexlowerp 'hexupperp))

(def 'hexp
  (labelp "hexadecimal"
    (plusp 'hexletterp 'digitp)))

(de stringp (String)
  (if String
    (let Stream (chop String)
      (mapp 'pack
        (seqp (charp (car Stream)) (stringp (pack (cdr Stream))))))
    (resultp "")))

(de manyp (@P)
  (fill '((State)
    ((plusp (seqp '@P (manyp '@P)) (resultp '())) State))))

(def 'wordp
  (mapp 'pack (manyp 'letterp)))

(def 'identp
  (seqp 'lowerp (manyp 'alphanump)))

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

(def 'spaces1p
  (many1p 'spacep))

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

# general parsing function
(de parse (P String)
  (let (*ParserInput (split (chop String) "\n" "\r")
        Result ((droplp 'spacesp P) (state)))
    (if (error? (car Result))
      (mapc 'pp> Result)
      (with (car Result) (: V)))))

# general definitions

(def 'digit
  'digitp)

(def 'alpha
  'letterp)

(def 'hexdig
  'hexp)

(def 'sp
  (charp " "))

(def 'htab
   (charp "\t"))

 (def 'cr
   (charp "\r"))

 (def 'crlf
   (charp "\n"))
