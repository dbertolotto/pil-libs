(symbols 'util 'pico)

(local) nil?
(de nil? (S)
  (= S NIL))

(local) nnil?
(de nnil? (S)
  (not (nil? S)))
