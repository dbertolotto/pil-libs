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

(de webpage ()
  (pack
    (:doctype)
    (:html
      (:head
        (:meta (attrs "charset" "UTF-8"))
        (:meta (attrs "name" "viewport"
                      "content" "width=device-width, initial-scale=1.0"))
        (:title "Hello title"))
      (:body
        (:h1 "Heading")
        (:p "hello html")))))

(out "web.html" (prinl (webpage)))
#(load "@lib/http.l" "@lib/xhtml.l")
#(server 8080 "!webpage")

