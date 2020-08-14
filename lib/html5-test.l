(load+ "#lib/html5.l")

(de test1 ()
  (println (attrs "a1" "v1" "a2" NIL "a3"))
  (mapc 'prinl (list
    (html-attr (cons "attr" "val"))
    (html-tag "mytag" NIL "some body")
    (html-tag "mytag" (attrs "attr1" NIL) "some body")
    (html-tag "mytag" (attrs "attr1" "val1") "some body")
    (html-tag "mytag" (attrs "attr1" "val1" "attr2" "val2") "some body")
    )))

(de test2 ()
  (mapc 'prinl (list
    (:doctype)
    )))

(test1)
(test2)

#(load "@lib/http.l" "@lib/xhtml.l")
#
(de webpage ()
  (pack
    (:doctype)
    (:html
      (:head
        (:meta (attrs "charset" "UTF-8"))
        (:title "Hello title"))
      (:body
        (:h1 "Heading")
        (:p "hello html")))))

(prinl (webpage))
##(server 8080 "!webpage")
#(prinl
#  (html 0 "Hello Web" "@lib.css" NIL
#    (:h1> NIL "Hello, web")
#    (:p> NIL "This is my first PicoLisp website")))


