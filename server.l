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

(de get-start-line ()
  (let (Start-Line (filter nnil? (splits (line) " "))
        Url (splits (cadr Start-Line) "?"))
    (list (intern (uppc (car Start-Line)))
          (lowc (car Url))
          (parse-params (cadr Url))
          (caddr Start-Line))))

(de get-headers ()
  (make
    (while (line T)
      (let (L (mapcar 'trims (splits @ ":")))
        (when L
          (link (cons (intern (lowc (car L))) (cadr L))))))))

(de get-body (Headers)
  (let (Length (format (cdr (assoc 'content-length Headers))))
    (when Length
      (pack (make (do Length (link (char))))))))
      #(pipe (echo Length) (till NIL T)))))

(class +HttpRequest)

(dm T (Start-line Headers Body)
  (=: Method (car Start-line))
  (=: Url (cadr Start-line))
  (=: Url-params (caddr Start-line))
  (=: Version (cadddr Start-line))
  (=: Headers Headers)
  (=: Body Body))

(dm method> () (: Method))
(dm url> () (: Url))
(dm url-params> () (: Url-params))
(dm version> () (: Version))
(dm headers> () (: Headers))
(dm body> () (: Body))
(dm params> () (append (: Url-params) (:Header)))

(class +HttpResponse)

(dm T (Version Status Message Headers Body)
  (=: Version Version)
  (=: Status Status)
  (=: Message Message)
  (=: Headers Headers)
  (=: Body Body))

(dm version> () (: Version))
(dm status> () (: Status))
(dm message> () (: Message))
(dm headers> () (: Headers))
(dm body> () (: Body))

(dm build> ()
  (pack (: Version) " " (: Status) " " (: Message) "\n"
        (mapcar '((P) (pack (car P) ": " (cdr P) "\n"      )) (: Headers))
        (when (: Body)
          "content-length: " (length (: Body)))
        "\n" (: Body)))

(de http-reader ()
  (new '(+HttpRequest) (get-start-line)
                       (get-headers)
                       (get-body Header)))

(de http-handler (Request)
  (let (Response-params '((content-type . "text/html; charset=utf-8"))
        Response-body (request-handler-2 Type Url Params Body))
    (http-response Version 200 'OK Response-params Response-body)))

(de request-handler (Type Url Params Body)
  (if (= Url "/greeting")
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

(de request-handler-2 (Type Url Params Body)
  (pack "<html>"
        (glue "<br>" (list Type
                           Url
                           (mapcar '((P) (pack (car P) ": " (cdr P) "<br>")) Params)
                           Body))
        "</html>"))

(de http-headers (Lst . Newline)
  (let (N (if Newline Newline "\n"))
    (mapcan '((P) (pack (car P) ": " (cdr P) N)) Lst)))

(de reader ()
  (mapcar pack (split
  (make (do 500
            (let (C (char))
              (! link C))
            (NIL (peek) NIL)
            ))
  "\n")))

(de handler (S)
  (let (Body (pack "<html>" S "</html>")
        Headers (http-headers (list (cons 'content-length (length Body)))))
    (http-response "HTTP/1.1" 200 'OK Headers Body)))

(de server (P)
  (task (port P)
    (when (accept @)
      (task @
            Sock @
            (in Sock
               (out Sock (prinl (*handler (*reader))))
               (prinl "Close task & sock " Sock)
               (task Sock)
               (close Sock))))))

(setq *handler http-handler)
(setq *reader http-reader)
(server 8080)

#(setq *handler handler)
#(setq *reader reader)
#(server 8080)

#(let (S (listen (port 8080))) (in S (do 250 (NIL (peek) NIL) (let (C (char)) (prinl C)))))

#(in S (make (do 250 (NIL (peek) NIL) (let (C (char)) (prinl C) (link C)))))
#(setq P (port 8080)) (let (S (listen P) Body (in S (make (do 200 (link (char)) (NIL (peek) NIL))))))
