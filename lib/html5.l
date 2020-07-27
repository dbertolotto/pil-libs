(setq *HtmlTags (list
  "html" "meta" "head" "body"
  "title" "p" "h1" "h2" "h3" "h4" "h5" "h6"
  ))

(de esc-char (C)
  (case C
    ("&" "&amp;")
    ("<" "&lt;")
    (">" "&gt;")
    ("\"" "&quot;")
    (T C)))

(de esc (Str)
  (pack (mapcar 'esc-char (chop Str))))

(de html-attr (A)
  (pack
    (car A)
    (when (cdr A)
      (pack "=" "\"" @ "\""))))

(de attrs @
  (make
    (while (args)
      (link (cons (next) (next))))))

(de html-tag (Name . @)
  (let (HasAttrs (lst? (next))
        Attrs (when HasAttrs (arg))
        Body (if HasAttrs
               (rest)
               (cons (arg) (rest))))
    (pack
      "<" Name
      (when Attrs " ")
      (glue " " (mapcar 'html-attr Attrs))
      (unless Body "/")
      ">" Body
      (when Body (pack "</" Name ">")))))

(de :doctype ()
  (html-tag "!DOCTYPE" (attrs "html")))

(de define-tag (@Tag)
  (let @TagFn (any (pack ":" @Tag))
    (macro (de @TagFn @ (pass 'html-tag @Tag)))))

(de generate-tag-fns ()
  (mapcar 'define-tag *HtmlTags))

(generate-tag-fns)

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
