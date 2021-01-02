(de de+ Rest
  (let ((@Name @Args Doc . @Body) Rest)
    (macro (de @Name @Args ~'@Body))
    (put @Name 'Doc Doc)
    Name))
(put 'de+ 'Doc
  "(de+ ...)

   Extended de function that allows a custom documentation string, stored as symbol property.")

(de md-to (Target)
  (pipe (out (list "cmark-gfm" "-t" Target)
             (prinl (get Sym 'Doc)))
        (till NIL T)))

(de+ doc+ (Sym)
  "(doc+ Sym)

   Extended doc function that shows the documentation string. The string is converted from markdown to html and shown in the browser"
  (out '(w3m -T text/html)
       (prinl (md-to 'html)))
  Sym)

(de+ help+ (Sym)
  "(help+ Sym)

   Extended help function that shows the documentation string. The string is converted from markdown to plain text and shown in the browser"
  (prinl (pack (need 40 "=")))
  (prin (md-to 'commonmark))
  (prinl (pack (need 40 "=")))
  Sym)

