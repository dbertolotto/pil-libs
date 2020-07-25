(de join (Lst Sep)
  (make
    (for (L Lst L)
      (link (pop 'L))
      (NIL L)
      (link Sep))))

### State

(class +Input)
(dm T (String)
  (=: I (split (chop String) "\n" "\r")))   # Input
(dm inp> ()
  (: I))

(de newinput (String)
  (new '(+Input) String))

(class +State)
(dm T (Inp Pos)
  (=: I Inp)                        # Input
  (=: P (if Pos Pos (cons 1 1))))   # Position
(dm row> (Pos)
  (car (nth (inp> (: I)) (cond
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
    (newstate (: I) Pos)))
(dm prev> ()
  (let ((R . C) (: P)
        Pos (if (= C 1)
              (cons (dec R) (inc (length (row> This (dec R)))))
              (cons R (dec C))))
    (newstate (: I) Pos)))

(de newstate (Inp Pos)
  (new '(+State) Inp Pos))

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
  (prin "Unexpected: ")
  (println (: M)))

(de error (Msg State Exp)
  (new '(+Error) Msg (prev> State) Exp))

(de error? (Result)
  (when (isa '(+Error) Result) T))

### Parser functions

## Core parsers

# return a value as parser
(de resultp (@V)
  (fill '((State)
    (list (success '@V State)))))

# this is used often, so it deserves a parser
(def 'nilp
  (resultp NIL))

# return an error with message
(de zerop (@Msg)
  (fill '((State)
    (list (error '@Msg State)))))

# add label in case of parser error
(de labelp (@Label @P)
  (fill '((State)
    (let (R ('@P State)
          R0 (car R))
      (if (error? R0)
        (mapcar '((X) (upd-exp> X @Label)) R)
        R)))))

# get next char and return next state
(def 'itemp
  (fill '((State)
    (if (car> State)
      (list (success (car> State) (next> State)))
      (list (error "No more input" State))))))

# bind
(de bindp (@P @F)
  (fill '((State)
    (let (R ('@P State)
          R0 (car R))
      (ifn (error? R0)
        (let (V (get R0 'V)
              S (get R0 'S))
          (('@F V) S))
        R)))))

# bind parsers in sequence and apply F to the results
(de bind-fun-mid (@P @F)
  (fill '((V)
          (push 'Vs V)
          (bindp '@P '@F))))

(de bind-fun-final (@F)
  (fill '((V)
          (push 'Vs V)
          (resultp (apply '@F (flip Vs))))))

(de generate-bind-parser @
  (let (Res (bind-fun-final (next))
        Plist (rest)
        Len (length Plist))
    (for (I . P) (flip Plist)
      (if (= I Len)
        (setq Res (bindp P Res))
        (setq Res (bind-fun-mid P Res))))))

(de bindnp @
  (let @P (pass 'generate-bind-parser)
    (fill '((State)
      (let Vs NIL
        ('@P State))))))

# sequence of parsers (cons'ed)
(de seqp @
  (pass 'bindnp 'cons))

# sequence of parsers (list'ed)
(de seqlstp @
  (pass 'bindnp 'list))

# satisfy predicate
(de satp (@Pred)
  (macro
    (bindp 'itemp
           '((X)
             (cond
               ((= X NIL) (zerop "No more input"))
               (('@Pred X) (resultp X))
               (T (zerop X)))))))

# apply P1 and if it returns an error apply P2 to the same state ('or' parser)
# shortcuts on the first successful parser
(de plus2p (@P1 @P2)
  (if (and @P1 @P2)
    (fill '((State)
      (let (R1 ('@P1 State)
            R10 (car R1))
        (if (error? R10)
          (let (R2 ('@P2 State)
                R20 (car R2))
            (if (error? R20)
              (append R2 R1)
              R2))
          R1))))
    (or @P1 @P2))) # return the single parser

# choice of parsers
(de plusp @
  (plus2p (next) (when (args) (pass 'plusp))))

## Derived parsers

# apply Fn to parser result
(de mapp (@Fn @P)
  (macro
    (bindp '@P '((V) (resultp ('@Fn V))))))

# parse 0 or more times the parser
(de manyp (@P)
  (fill '((State)
    ((plusp (seqp '@P (manyp '@P)) 'nilp) State))))

# parse 1 or more times the parser
(de many1p (P)
  (seqp P (manyp P)))

# parses P between X and Y times
(de manyxyp (P X Y)
  (let (Seqs (append (maplist '((L) (cons 'seqp L)) (need Y P))
                     (list 'nilp))
        Cut (head (inc (- Y X)) Seqs))
    (eval (cons 'plusp Cut))))

# parse in sequence and drop the leftmost parsed value
(de droplp (Pl Pr)
  (bindnp '((Vl Vr) Vr) Pl Pr))

# parse in sequence and drop the rightmost parsed value
(de droprp (Pl Pr)
  (bindnp '((Vl Vr) Vl) Pl Pr))

# parse between left and right parsers and drop them
(de betweenp (Pl Pc Pr)
  (bindnp '((Vl Vc Vr) Vc) Pl Pc Pr))

# parses with separators (and drop them) 1 or more times
(de sepby1p (P Sep)
  (seqp P (manyp (droplp Sep P))))

# parses with separators (and drop them) 0 or more times
(de sepbyp (P Sep)
  (plusp (sepby1p P Sep) 'nilp))

# ignores parser P result and replaces it with W
(de ignorep (@P @W)
  (macro
    (bindp '@P '((V) (resultp '@W)))))

# optional parser
(de optp (P)
  (plusp P 'nilp))

## Generic parsing function

(de parse (P String DropLeadSpaces)
  (let (Inp (newinput String)
        Res ((if DropLeadSpaces
               (droplp 'spacesp P)
               P)
             (newstate Inp)))
    (if (error? (car Res))
      (mapc 'pp> Res)
      (with (car Res) (: V)))))

## common parser definitions

# character
(de charp (@C)
  (macro
    (labelp (text "char '@1'" @C)
      (satp '((X) (= @C X))))))

# string
(de stringp (String)
  (mapp 'pack
    (apply 'seqp (mapcar 'charp (chop String)))))

# char range parser
(de crangep (@Low @Upp)
  (macro
    (satp '((X) (and (>= X @Low) (<= X @Upp))))))

# unicode range parser
(de urangep (@Low @Upp)
  (macro
    (satp '((X) (and (>= X (char (hex @Low))) (<= X (char (hex @Upp))))))))

# member of list parser
(de memberp (@Lst)
  (macro
    (satp '((X) (member X '@Lst)))))

# digit
(def 'digitp
  (labelp "digit"
    (crangep "0" "9")))

# lowercase letter
(def 'lowerp
  (crangep "a" "z"))

# uppercase letter
(def 'upperp
  (crangep "A" "Z"))

# hex lowercase letter
(def 'hexlowerp
  (crangep "a" "f"))

# hex uppercase letter
(def 'hexupperp
  (crangep "A" "F"))

# any 'space'
(def 'spacep
  (memberp (chop " \t\n\r")))

# 0 or more 'spaces'
(def 'spacesp
  (manyp 'spacep))

# 1 or more 'spaces'
(def 'spaces1p
  (many1p 'spacep))

# letter
(def 'letterp
  (plusp 'lowerp 'upperp))

# alphanumeric
(def 'alphanump
  (labelp "alphanumeric"
    (plusp 'letterp 'digitp)))

# hex letter
(def 'hexletterp
  (plusp 'hexlowerp 'hexupperp))

# hexadecimal
(def 'hexp
  (labelp "hexadecimal"
    (plusp 'hexletterp 'digitp)))

## Aliases

(def 'digit
  'digitp)

(def 'alpha
  'letterp)

(def 'alphanum
  'alphanump)

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
