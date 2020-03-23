(symbols 'util 'pico)

(local) nil?
(de nil? (S)
  (= S NIL))
(test T (nil? NIL))
(test T (nil? '()))
(test T (nil? ""))
(test NIL (nil? 'a))
(test NIL (nil? "a"))
(test NIL (nil? 1))

(local) nnil?
(de nnil? (S)
  (not (nil? S)))
(test T (nnil? 'a))
(test NIL (nnil? NIL))
