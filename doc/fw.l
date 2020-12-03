(de de+ Rest
  (let ((@Name @Args Doc . @Body) Rest)
    (macro (de @Name @Args ~'@Body))
    (put @Name 'Doc Doc)
    Name))

(de+ f (X)
  "Function that adds 1 to X
   - X input
   Does not do much in the end"
   (+ X 1))

(de doc+ (Sym)
  (pipe (out '(pandoc --from=markdown --to=html -s --metadata title=Doc) (prinl (get Sym 'Doc))) (call 'w3m)))

