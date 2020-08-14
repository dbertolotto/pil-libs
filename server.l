(load+ "#lib/string.l" "#lib/util.l" "#lib/classutil.l" "#lib/server.l" "#parser/http.l" "#lib/html5.l" "#lib/http.l")
(symbols '(pico string util classutil))

# Unreserved URL characters
(setq *Unreserved "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~")

(setq
  *MaxReqLineLength (* 8 1024)
  *MaxHeaderLength (* 8 1024)
  *MaxBodyLength (* 8 1024))

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
          (cadr Url)
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

(de read-body (Headers)
  (let (Length (format (cdr (assoc 'content-length Headers)))
        Cnt 0)
    (when Length
    (pack
      (make
        (loop
          (setq Cnt (inc Cnt))
          (T (> Cnt Length) (throw 'HttpContentLengthException))
          (T (> Cnt *MaxBodyLength) (throw 'HttpBodyLengthException))
          (link (char))))))))

(de read-headers ()
  (let Cnt 0
    (make
      (loop
        (link
          (make
            (loop
              (link (char))
              (setq Cnt (inc Cnt))
              (T (> Cnt *MaxHeaderLength) (throw 'HttpHeaderLengthException))
              (T (= (peek) "\r") (char) (char)))))
        (T (= (peek) "\r") (line))))))

(de parse-headers (Streams)
  (make
    (while (pop 'Streams)
      (let (L (mapcar 'trims (splits @ ":")))
        (when L
          (link (cons (intern (lowc (car L))) (cadr L))))))))

(de read-request-line ()
  (let Cnt 0
    (make
      (loop
        (link (char))
        (setq Cnt (inc Cnt))
        (T (> Cnt *MaxReqLineLength) (throw 'HttpReqLineLengthException))
        (T (= (peek) "\r") (char) (char))))))

(de parse-request-line (Stream)
  (let (Parts (split Stream " ")
        Method (pack (car Parts))
        Uri (split (cadr Parts) "?" "#")
        Path (pack (car Uri))
        Query (pack (cadr Uri))
        Fragment (pack (caddr Uri))
        Version (pack (caddr Parts)))
    (list Method (http-uri Path Query Fragment) Version)))

(de http-reader ()
  (let (Start-line (parse-request-line (read-request-line))
        Method (car Start-line)
        Uri (cadr Start-line)
        Version (caddr Start-line)
        Headers (parse-headers (read-headers))
        Body (read-body Headers))
    (http-req Method Uri Version Headers Body)))

(de http-headers (Lst . @)
  (let (N (if (next) (arg) "\n"))
    (mapcar '((P) (pack (car P) ": " (cdr P) N)) Lst)))

### Other stuff

(de http-handler-demo (Request)
  (let (Response-params '((content-type . "text/html; charset=utf-8"))
        Uri (get Request 'Uri)
        Response-body (request-handler (get Request 'Method) (get Uri 'Path) (params> Uri) (get Request Body)))
    (prinl (build> (http-res (get Request 'Version) 200 'OK Response-params Response-body)))))

(de request-handler (Method Path Params Body)
  (if (= Path "/greeting")
    (let (Name (assoc 'name Params))
      (if Name
        (text "<html>Nice to meet you, @1!</html>" (cdr Name))
        "<html><form>What is your name?<input name='name' /></form></html>"))
    "<html>Sorry... I don't know that page.</html>"))

### simple echo handler

(de http-handler-echo (Request)
  (let (Response-params '((content-type . "text/html; charset=utf-8"))
        Built-req (let *HttpNewLine (:br) (build> Request))
        Response-body (html5 "Echoing request" NIL Built-req))
    (prinl (build> (http-res (get Request 'Version) 200 'OK Response-params Response-body)))))

### start server

(de http-echo-server (Port)
  (server Port 'http-reader 'http-handler-echo))
(http-echo-server 8080)
