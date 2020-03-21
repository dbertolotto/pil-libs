(de xml-attr (a)
  (pack
    (car a)
    (if (cdr a) (pack "=\"" (cdr a) "\""))))

(de attrs @
  (make
    (while (args)
      (link (cons (next) (next))))))

(de xml-tag (name . @)
  (let (attrs (next)
        body (rest))
    (pack
      (pass 'pack
        "<" name
        (if attrs " ")
        (glue " " (mapcar 'xml-attr attrs))
        (ifn body "/")
        ">")
      (if body (pack "</" name ">")))))

(de <doctype> ()
  (xml-tag "!DOCTYPE" (list (cons "html" NIL))))

(setq html5-tags (list
  "html" "meta" "head" "body"
  "title" "p" "h1" "h2" "h3" "h4" "h5" "h6"
  ))

(mapcar 'eval
  (mapcar
    '((tag)
      (let (@tag tag
            @ftag (any (pack "<" tag ">")))
        (fill '(de @ftag @ (pass 'xml-tag @tag)) '(@ftag @tag))))
    html5-tags))

(de test1 ()
  (mapc 'prinl (list
    (xml-attr (cons "attr" "val"))
    (attrs "a1" "v1" "a2" NIL "a3")
    (xml-tag "mytag" NIL "some body")
    (xml-tag "mytag" (attrs "attr1" NIL) "some body")
    (xml-tag "mytag" (attrs "attr1" "val1") "some body")
    (xml-tag "mytag" (attrs "attr1" "val1" "attr2" "val2") "some body")
    )))

(de test2 ()
  (mapc 'prinl (list
    (<doctype>)
    )))

(test1)
(test2)

(load "@lib/http.l" "@lib/xhtml.l")

(de webpage ()
  (pack
    (<doctype>)
    (<html> NIL
      (<head> NIL
        (<meta> (attrs "charset" "UTF-8"))
        (<title> NIL "Hello title"))
      (<body> NIL
        (<h1> NIL "Heading")
        (<p> NIL "hello html")))))

(prinl (webpage))
#(server 8080 "!webpage")
(prinl
  (html 0 "Hello Web" "@lib.css" NIL
    (<h1> NIL "Hello, web")
    (<p> NIL "This is my first PicoLisp website")))
