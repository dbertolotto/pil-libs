(de de+ Rest
  (let ((@Name @Args Doc . @Body) Rest)
    (macro (de @Name @Args ~'@Body))
    (put @Name 'Doc Doc)
    Name))

(de+ f (X)
  "(f num)

   Function that adds 1 to X.

   In/out
   - X input
   - Y output

   Does not do much in the end."
   (+ X 1))

(de doc+ (Sym)
  (out '(w3m -T text/html)
       #(prinl (pipe (out '(pandoc -f markdown+lists_without_preceding_blankline -t html -s --metadata title=Doc)
       (prinl (pipe (out (list "pandoc" "-f" "markdown_github" "-t" "html" "-s" "--metadata" (pack "title=" Sym " documentation"))
                         (prinl (get Sym 'Doc)))
                    (till NIL T)))))

(de help+ (Sym)
  (prinl (pack (need 40 "=")))
  (prinl (pipe (out (list "pandoc" "-f" "markdown_github" "-t" "plain" "-s" "--metadata" (pack "title=" Sym " documentation"))
                    (prinl (get Sym 'Doc)))
               (till NIL T)))
  (prinl (pack (need 40 "=")))
  Sym)

