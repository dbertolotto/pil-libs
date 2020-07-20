(load "parser.l")

### XML

(def 'xcharp
  (satp '((X)
           (or
             (and (>= X `(uni "1"))
                  (<= X `(uni "d7ff")))
             (and (>= X `(uni "e000"))
                  (<= X `(uni "fffd")))
             (and (>= X `(uni "10000"))
                  (<= X `(uni "10ffff")))))))

(def 'xrestrictedcharp
  (satp '((X)
           (or
             (and (>= X `(uni "1"))
                  (<= X `(uni "8")))
             (and (>= X `(uni "b"))
                  (<= X `(uni "c")))
             (and (>= X `(uni "e"))
                  (<= X `(uni "1f")))
             (and (>= X `(uni "7f"))
                  (<= X `(uni "84")))
             (and (>= X `(uni "86"))
                  (<= X `(uni "9f")))))))

(def 'xspacep
  (satp '((X) (member X `(list (uni "20") (uni "9") (uni "d") (uni "a"))))))

(def 'xspacesp
  (manyp xspacep))

(def 'xspaces1p
  (many1p xspacep))

(def 'xset1p
  (satp '((X)
           (or
             (and (>= X `(uni "c0"))
                  (<= X `(uni "d6")))
             (and (>= X `(uni "d8"))
                  (<= X `(uni "f6")))
             (and (>= X `(uni "f8"))
                  (<= X `(uni "2ff")))
             (and (>= X `(uni "370"))
                  (<= X `(uni "37d")))
             (and (>= X `(uni "37f"))
                  (<= X `(uni "1fff")))
             (and (>= X `(uni "200c"))
                  (<= X `(uni "200d")))
             (and (>= X `(uni "2070"))
                  (<= X `(uni "218f")))
             (and (>= X `(uni "2c00"))
                  (<= X `(uni "2fef")))
             (and (>= X `(uni "3001"))
                  (<= X `(uni "d7ff")))
             (and (>= X `(uni "f900"))
                  (<= X `(uni "fdcf")))
             (and (>= X `(uni "fdf0"))
                  (<= X `(uni "fffd")))
             (and (>= X `(uni "10000"))
                  (<= X `(uni "effff")))))))

(def 'xset2p
  (satp '((X)
           (or
             (= X `(uni "b7"))
             (and (>= X `(uni "0300"))
                  (<= X `(uni "036f")))
             (and (>= X `(uni "203f"))
                  (<= X `(uni "2040")))))))

(def 'xnamestartcharp
  (plusp 'letterp (charp ":") (charp "_") 'xset1p))

(def 'xnamecharp
  (plusp 'xnamestartcharp (charp "-") (charp ".") 'xset2p))

(def 'xnamep
  (plusp 'xnamestartcharp (manyp 'xnamecharp)))

(def 'xnamesp
  (sepby1p 'xnamep (charp " ")))

(def 'xnmtokenp
  (many1p 'xnamecharp))

(def 'xnmtokensp
  (sepby1p 'xnmtokenp (charp " ")))

(def 'xcharref
  (plusp
    (seqp (charp "&") (charp "#") (many1p 'digitp) (charp ";"))
    (seqp (charp "&") (charp "#") (charp "x") (many1p 'alphanum) (charp ";"))))

(def 'xentityrefp
  (seqp (charp "&") 'xnamep (charp ";")))

(def 'xpereferencep
  (seqp (charp "%") 'xnamep (charp ";")))

(def 'xreferencep
  (plusp 'xentityrefp 'xcharrefp))
(bye)
(def 'xentityvaluep
  (let (dquote (charp "\"")
        quote (charp "'")
        symbol (plusp (charp "\^") (charp "%") (charp "&"))
        value (manyp (plusp symbol 'xpereferencep 'xreferencep)))
    (plusp
      (betweenp dquote value dquote)
      (betweenp quote value quote))))

(def 'xattvaluep
  (let (dquote (charp "\"")
        quote (charp "'")
        symbol (plusp (charp "\^") (charp "<") (charp "&"))
        value (manyp (plusp symbol 'xreferencep)))
    (plusp
      (betweenp dquote value dquote)
      (betweenp quote value quote))))
