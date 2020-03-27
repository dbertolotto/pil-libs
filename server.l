(load "lib/string.l"
      "lib/util.l")
(symbols '(pico string util))

(de decode-char (C1 C2)
  (char (hex (pack C1 C2))))

(de decode-param (S)
  (pack
    (make
      (for (L (chop S) L)
        (let (C (pop 'L))
          (link
            (case C
              ("%" (decode-char (pop 'L) (pop 'L)))
              ("+" " ")
              (T C))))))))

(de parse-param (S)
  (let (Pair (splits S "="))
    (cons (intern (lowc (car Pair)))
          (decode-param (cadr Pair)))))

(de parse-params (S)
  (mapcar 'parse-param (splits S "&")))

(de get-url ()
  (let (Parts (filter nnil? (splits (line) " "))
        Url (splits (cadr Parts) "?"))
    (list (intern (uppc (car Parts)))
          (lowc (car Url))
          (parse-params (cadr Url))
          (caddr Parts))))

(de get-header ()
  (make
    (while (line T)
      (let (L (mapcar 'trims (splits @ ":")))
        (when L
          (link (cons (intern (lowc (car L))) (cadr L))))))))

(de get-body (Header)
  (let (Length (format (cdr (assoc 'content-length Header))))
    (when Length
      (pipe (echo Length) (till NIL T)))))

(de server (P)
  (task (port P)
    (when (accept @)
      (task @
            Sock @
            (in Sock
               (out Sock (prinl (http-handler (http-reader))))
               (prinl "Close task & sock " Sock)
               (task Sock)
               (close Sock))))))

(de http-reader ()
  (let (Url (get-url)
        Header (get-header)
        Body (get-body Header))
    (list Url Header Body)))

(de http-handler (S)
  (let (Url (car S)
        Type (car Url)
        Path (cadr Url)
        Url-params (caddr Url)
        Version (cadddr Url)
        Header (cadr S)
        Body (caddr S)
        Params (append Url-params Header)
        Response-params '((content-type . "text/html; charset=utf-8"))
        Response-body (request-handler-2 Type Path Params Body))
    (http-response Version 200 'OK Response-params Response-body)))

(de request-handler (Type Path Params Body)
  (if (= Path "/greeting")
    (let (Name (assoc 'name Params))
      (if Name
        (text "<html>Nice to meet you, @1!</html>" (cdr Name))
        "<html><form>What is your name?<input name='name' /></form></html>"))
    "<html>Sorry... I don't know that page.</html>"))

(de http-response (Version Status Message Header Body)
  (pack Version " " Status " " Message "\n"
        (mapcar '((P) (pack (car P) ": " (cdr P) "\n")) Header)
        "\n"
        Body))

(de request-handler-2 (Type Path Params Body)
  (pack "<html>"
        (glue "<br>" (list Type
                           Path
                           (mapcar '((P) (pack (car P) ": " (cdr P) "<br>")) Params)
                           Body))
        "</html>"))

(server 8080)
