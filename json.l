(local) '(read-json print-json readJsonStr)

(setq *Indent 2)
(setq *Pretty NIL)

(de check-json (X Item)
  (unless (= X Item)
    (quit 'JsonParsingException
          (pack "Bad JSON token: '" Item
                "' - expected '" X "'"))))

(de read-json ()
  (case (read "_")
    ("{"
      (make
        (for (X (read-json) (not (= "}" X)) (read-json))
          (check-json ":" (read-json))
          (link (cons X (read-json)))
          (T (= "}" (setq X (read-json))))
          (check-json "," X))))
    ("["
      (make
        (link T) # Array marker
        (for (X (read-json) (not (= "]" X)) (read-json))
          (link X)
          (T (= "]" (setq X (read-json))))
          (check-json "," X))))
    (T
      (let X @
        (cond
          ((pair X) (pack X))
          ((and (= "-" X) (format (peek))) (read-exp (read) T))
          ((and (= "+" X) (format (peek))) (read-exp (read)))
          ((format X) (read-exp X))
          (T X))))))

(de read-exp (X Neg)
  (let (Exp (when (= "e" (lowc (peek)))
              (char)
              (let E (read "_")
                (cond
                  ((and (= "-" E) (format (peek))) (- (read)))
                  ((and (= "+" E) (format (peek))) (read))
                  ((format E) E)
                  (T (handle-error)))))
        Num (if Exp (apply-exp X Exp) X))
    (if Neg (- Num) Num)))

(de apply-exp (X Exp)
  (let (Fact (apply * (need (abs Exp) 10)))
    (cond
      ((> Exp 0) (* X Fact))
      ((= Exp 0) X)
      ((< Exp 0) (/ X Fact)))))

(de print-json (Item Indent)
  (let (IndStr (when *Pretty (pack (need Indent " ")))
        NewInd (+ Indent *Indent)
        NewIndStr (when *Pretty (pack (need NewInd " ")))
        Newline (when *Pretty "\n"))
    (cond
      ((atom Item)
        (if Item
          (prog
            (prin NewIndStr)
            (print Item))
          (prin "{}")))
      ((=T (car Item))
        (prin "[" Newline)
        (map '((X)
                (print-json (car X) Indent)
                (and (cdr X) (prin ", " Newline)))
             (cdr Item))
        (prin Newline IndStr "]"))
      (T
        (prin "{" Newline)
        (map '((X)
                (prin NewIndStr)
                (print (caar X))
                (prin ": ")
                (print-json (cdar X) NewInd)
                (and (cdr X) (prin ", " Newline)))
             Item)
        (prin Newline IndStr "}")))))

(de print-json (Item)
  (let *Pretty NIL (print-json Item NIL)))

(de pprint-json (Item)
  (let *Pretty T (print-json Item 0)))

(de pretty-print (String)
  (let (S (chop String)
        Ind 0)
    (pack
    (make
      (for C S
        (case C
          ("," (link C "\n" (need Ind " ")))
          ("{" ((setq Ind (+ Ind *Indent))
                (link C "\n" (need Ind " "))))
          ("}" ((setq Ind (- Ind *Indent))
                (link C "\n" (need Ind " "))))
          (T (link C))))))))


(de readJsonStr (S)
  (pipe (prinl S) (read-json)))

#----------

(de _jget (J K)
  (if (num? K)
    (car (nth J K))
    (let (E (assoc K J))
      (cond
        ((not E) NIL)
       b((and (lst? (cdr E)) (=T (cadr E))) (cddr E))
        (T (cdr E))))))

(de jget (J . @)
  (let (R (_jget J (next)))
    (if (args)
      (pass 'jget R)
      R)))

(de jkeys (J)
  (mapcar 'car J))

(de jvals (J)
  (mapcar 'cdr J))

### Tests

