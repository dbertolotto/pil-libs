(de join (Lst Sep)
  (make
    (for (L Lst L)
      (link (pop 'L))
      (NIL L)
      (link Sep))))

(de uni (Str)
  (char (hex Str)))

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
    #(prin> State)
    (let (R ('@P State)
          R0 (car R))
      (ifn (error? R0)
        (let (V (get R0 'V)
              S (get R0 'S))
          (('@F V) S))
        R)))))

# bind two parser in sequence
(de bind2p (@F @P1 @P2)
  (eval
    (fill
      '(bindp '@P1 '((@V1)
        (bindp '@P2 (fill '((V2)
          (resultp ('@F '@V1 V2)))
        '@V1))))
      '(@F @P1 @P2))))

(de bind3p (@F @P1 @P2 @P3)
  (eval
    (fill
      '(bindp '@P1 '((@V1)
                     (bindp '@P2 (fill '((@V2)
                                        (bindp '@P3 (fill '((V3)
                                                          (resultp ('@F '@V1 '@V2 V3)))
                                                          '@V2)))
                                       '@V1))))
      '(@F @P1 @P2 @P3))))

# variant #2
(de bind2p (@F @P1 @P2)
  (fill '((State)
    (let Vs NIL
      ((bindp '@P1 '((V) (push 'Vs V)
         (bindp '@P2 '((V) (push 'Vs V)
           (resultp (apply '@F (flip Vs))))))) State)))))

(de bind3p (@F @P1 @P2 @P3)
  (fill '((State)
    (let Vs NIL
      ((bindp '@P1 '((V) (push 'Vs V)
         (bindp '@P2 '((V) (push 'Vs V)
           (bindp '@P3 '((V) (push 'Vs V)
             (resultp (apply '@F (flip Vs))))))))) State)))))

# variant #3
(de bind-fun-mid (@P @F)
  (fill '((V)
          (push 'Vs V)
          (bindp '@P '@F))))

(de bind-fun-final (@F)
  (fill '((V)
          (push 'Vs V)
          (resultp (apply '@F (flip Vs))))))

(de bind2p (@F @P1 @P2)
  (fill '((State)
    (let (Vs NIL
          Parser (bindp '@P1 (bind-fun-mid '@P2 (bind-fun-final '@F))))
      (Parser State)))))

(de bind3p (@F @P1 @P2 @P3)
  (fill '((State)
    (let (Vs NIL
          Parser (bindp '@P1 (bind-fun-mid '@P2 (bind-fun-mid '@P3 (bind-fun-final '@F)))))
      (Parser State)))))

(de bind4p (@F @P1 @P2 @P3 @P4)
  (fill '((State)
    (let (Vs NIL
          Parser (bindp '@P1 (bind-fun-mid '@P2 (bind-fun-mid '@P3 (bind-fun-mid '@P4 (bind-fun-final '@F))))))
      (Parser State)))))

(de bind5p (@F @P1 @P2 @P3 @P4 @P5)
  (fill '((State)
    (let (Vs NIL
          Parser (bindp '@P1 (bind-fun-mid '@P2 (bind-fun-mid '@P3 (bind-fun-mid '@P4 (bind-fun-mid '@P5 (bind-fun-final '@F)))))))
      (Parser State)))))

(de bind6p (@F @P1 @P2 @P3 @P4 @P5 @P6)
  (fill '((State)
    (let (Vs NIL
          Parser (bindp '@P1 (bind-fun-mid '@P2 (bind-fun-mid '@P3 (bind-fun-mid '@P4 (bind-fun-mid '@P5 (bind-fun-mid '@P6 (bind-fun-final '@F))))))))
      (Parser State)))))

(de bind7p (@F @P1 @P2 @P3 @P4 @P5 @P6)
  (fill '((State)
    (let (Vs NIL
          Parser (bindp '@P1 (bind-fun-mid '@P2 (bind-fun-mid '@P3 (bind-fun-mid '@P4 (bind-fun-mid '@P5 (bind-fun-mid '@P6 (bind-fun-mid '@P7 (bind-fun-final '@F)))))))))
      (Parser State)))))

# variant #4
(de generate-bind-parser @
  (let (Res (bind-fun-final (next))
        Plist (rest)
        Len (length Plist))
    (for (I . P) (flip Plist)
      (if (= I Len)
        (setq Res (bindp P Res))
        (setq Res (bind-fun-mid P Res))))))

(de bindnp @Args
  (fill '((State)
    (let (Vs NIL
          Parser (generate-bind-parser ~'@Args))
      (Parser State)))))

(de bindnp @
  (let @Args (rest)
    (fill '((State)
      (let (Vs NIL
            Parser (generate-bind-parser ~'@Args))
        (Parser State))))))

# apply two parser in sequence and cons the result ('and' parser)
(de seq2p (P1 P2)
  (if (and P1 P2)
    (bind2p 'cons P1 P2)
    (or P1 P2))) # return the single parser

# sequence of parsers
(de seqp @
  (seq2p (next) (when (args) (pass 'seqp))))
#(de seqp @
#  (eval (cons 'bindnp 'cons (rest))))
(de oldseqp @
  (seq2p (next) (when (args) (pass 'oldseqp))))
(de seqp @
  (let Lst (rest)
    (case (length Lst)
      (0 NIL)
      (1 (car Lst))
      (2 (apply 'bind2p Lst 'cons))
      (3 (apply 'bind3p Lst 'cons))
      (4 (apply 'bind4p Lst 'cons))
      (5 (apply 'bind5p Lst 'cons))
      (6 (apply 'bind6p Lst 'cons))
      (7 (apply 'bind7p Lst 'cons))
      (T (apply 'oldseqp Lst)))))
#(de seqp @Args
#  (fill '((State)
#    (let (Vs NIL
#          Parser (generate-bind-parser 'cons ~'@Args))
#      (Parser State)))))
#(de seqp @
#  (pass 'bindnp 'cons))

# apply two parser in sequence and use list to join the result ('and' parser)
# sometimes a list is better that cons
(de seqlst2p (P1 P2)
  (if (and P1 P2)
    (bind2p 'list P1 P2)
    (or P1 P2))) # return the single parser

# sequence-list of parsers
(de seqlstp @
  (seqlst2p (next) (when (args) (pass 'seqlstp))))
#(de seqlstp @
#  (eval (cons 'bindnp 'list (rest))))
(de oldseqlstp @
  (seqlst2p (next) (when (args) (pass 'oldseqlstp))))
(de seqlstp @
  (let Lst (rest)
    (case (length Lst)
      (0 NIL)
      (1 (car Lst))
      (2 (apply 'bind2p Lst 'list))
      (3 (apply 'bind3p Lst 'list))
      (4 (apply 'bind4p Lst 'list))
      (5 (apply 'bind5p Lst 'list))
      (6 (apply 'bind6p Lst 'list))
      (7 (apply 'bind7p Lst 'list))
      (T (apply 'oldseqlstp Lst)))))

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
(de droplp (P1 P2)
  (bind2p '((V1 V2) V2) P1 P2))

# parse in sequence and drop the rightmost parsed value
(de droprp (P1 P2)
  (bind2p '((V1 V2) V1) P1 P2))

# parse between left and right parsers and drop them
(de betweenp (Pleft P Pright)
  (droplp Pleft (droprp P Pright)))

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

# digit
(def 'digitp
  (labelp "digit"
    (satp '((X) (and (>= X "0") (<= X "9"))))))

# lowercase letter
(def 'lowerp
  (satp '((X) (and (>= X "a") (<= X "z")))))

# uppercase letter
(def 'upperp
  (satp '((X) (and (>= X "A") (<= X "Z")))))

# hex lowercase letter
(def 'hexlowerp
  (satp '((X) (and (>= X "a") (<= X "f")))))

# hex uppercase letter
(def 'hexupperp
  (satp '((X) (and (>= X "A") (<= X "F")))))

# any 'space'
(def 'spacep
  (satp '((X) (member X '(" " "\t" "\n" "\r")))))

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
