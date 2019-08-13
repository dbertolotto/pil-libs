(local) '(checkJson readJson printJson readJsonStr)

(de checkJson (X Item)
   (unless (= X Item)
      (quit "Bad JSON" Item) ) )
 
(de readJson ()
   (case (read "_")
      ("{"
         (make
            (for (X (readJson)  (not (= "}" X))  (readJson))
               (checkJson ":" (readJson))
               (link (cons X (readJson)))
               (T (= "}" (setq X (readJson))))
               (checkJson "," X) ) ) )
      ("["
         (make
            (link T)  # Array marker
            (for (X (readJson)  (not (= "]" X))  (readJson))
               (link X)
               (T (= "]" (setq X (readJson))))
               (checkJson "," X) ) ) )
      (T
         (let X @
            (cond
               ((pair X) (pack X))
               ((and (= "-" X) (format (peek)))
                  (- (read)) )
               (T X) ) ) ) ) )
 
(de printJson (Item)  # For simplicity, without indentation
   (cond
      ((atom Item) (if Item (print @) (prin "{}")))
      ((=T (car Item))
         (prin "[")
         (map
            '((X)
               (printJson (car X))
               (and (cdr X) (prin ", ")) )
            (cdr Item) )
         (prin "]") )
      (T
         (prin "{")
         (map
            '((X)
               (print (caar X))
               (prin ": ")
               (printJson (cdar X))
               (and (cdr X) (prin ", ")) )
            Item )
         (prin "}") ) ) )

(de readJsonStr (S)
  (pipe (prinl S) (readJson)))

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
