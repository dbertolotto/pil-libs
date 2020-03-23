(load "util.l")
(symbols '(pico util))

(de test-nil? ()
  (test T (nil? NIL))
  (test T (nil? '()))
  (test T (nil? ""))
  (test NIL (nil? 'a))
  (test NIL (nil? "a"))
  (test NIL (nil? 1)))

(de test-nnil? ()
  (test T (nnil? 'a))
  (test NIL (nnil? NIL)))

(de tests ()
  (test-nil?)
  (test-nnil?))
