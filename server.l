(load "lib/string.l" "lib/util.l" "lib/classutil.l" "lib/server.l")
(symbols '(pico string util classutil))

# Unreserved URL characters
(setq *Unreserved "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~")

(de decode-char (C1 C2)
  (char (hex (pack C1 C2))))

(de encode-char (C)
  (hex (char C)))

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

(de url-encode (S)
  (let (Unres (chop *Unreserved))
    (pack
      (mapcar '((C) (if (member C Unres)
                      C
                      (pack "%" (encode-char C))))
              (chop S)))))

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
      #(pack (make (do Length (link (char))))))))
      (pipe (echo Length) (till NIL T)))))

(class +HttpRequest)

(dm T (Start-line Headers Body)
  (=: Method (car Start-line))
  (=: Url (cadr Start-line))
  (=: Url-params (caddr Start-line))
  (=: Version (cadddr Start-line))
  (=: Headers Headers)
  (=: Body Body))

(&get Method Url Url-params Version Headers Body)

(dm params> () (append (: Url-params) (: Headers)))

(dm build> @
  (let (N (if (next) (arg) "\n"))
    (pack (: Method) " " (: Url) " " (: Version) N
          (http-headers (params> This) N)
          (when (: Body)
            (pack "content-length: " (length (: Body)) N))
          N
          (: Body))))

(class +HttpResponse)
(&con Version Status Message Headers Body)
(&get Version Status Message Headers Body)

(dm build> ()
  (pack (: Version) " " (: Status) " " (: Message) "\n"
        (http-headers (: Headers))
        (when (: Body)
          (pack "content-length: " (length (: Body)) "\n"))
        "\n"
        (: Body)))

(de http-reader ()
  (let (Start-line (get-start-line)
        Headers (get-headers)
        Body (get-body Headers))
    (new '(+HttpRequest) Start-line Headers Body)))

(de http-headers (Lst . @)
  (let (N (if (next) (arg) "\n"))
    (mapcar '((P) (pack (car P) ": " (cdr P) N)) Lst)))

### Other stuff

(de http-handler-demo (Request)
  (let (Response-params '((content-type . "text/html; charset=utf-8"))
        Response-body (request-handler (method> Request) (url> Request) (params> Request) (body> Request)))
    (prinl (build> (new '(+HttpResponse) (version> Request) 200 'OK Response-params Response-body)))))

(de request-handler (Method Url Params Body)
  (if (= Url "/greeting")
    (let (Name (assoc 'name Params))
      (if Name
        (text "<html>Nice to meet you, @1!</html>" (cdr Name))
        "<html><form>What is your name?<input name='name' /></form></html>"))
    "<html>Sorry... I don't know that page.</html>"))

### simple echo handler

(de http-handler-echo (Request)
  (let (Response-params '((content-type . "text/html; charset=utf-8"))
        Response-body (pack "<html>" (build> Request "<br>") "</html>"))
    (prinl (build> (new '(+HttpResponse) (version> Request) 200 'OK Response-params Response-body)))))

### start server

(de http-echo-server (Port)
  (server Port 'http-reader 'http-handler-echo))
(http-echo-server 8080)
