(load+ "#lib/string.l" "#lib/util.l" "#lib/classutil.l" "#lib/server.l" "#parser/http.l" "#lib/html5.l" "#lib/http.l")
(symbols '(pico server http string util classutil))

(setq
  *MaxReqLineLength (* 8 1024)
  *MaxHeaderLength (* 8 1024)
  *MaxBodyLength (* 8 1024))

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

(de get-content-length (Headers)
  (format (cdr (assoc 'content-length Headers))))

(de read-body (Headers)
  (let (Length (get-content-length Headers))
    (when (> Length *MaxBodyLength)
      (throw 'HttpBodyLengthException))
    (when Length
      (pipe (echo Length) (till)))))

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
  (catch 'HttpReqLineLengthException
  (let (Start-line (parse-request-line (read-request-line))
        Method (car Start-line)
        Uri (cadr Start-line)
        Version (caddr Start-line)
        Headers (parse-headers (read-headers))
        Body (read-body Headers))
    (http-req Method Uri Version Headers Body))))

(de http-headers (Lst . @)
  (let (N (if (next) (arg) "\n"))
    (mapcar '((P) (pack (car P) ": " (cdr P) N)) Lst)))

### Other stuff

(de http-handler-demo (Request)
  (let (Response-params '((content-type . "text/html; charset=utf-8"))
        Uri (get Request 'U)
        Response-body (request-handler (get Request 'M) (get Uri 'P) (params> Uri) (get Request 'B)))
    (prin (build> (http-res (get Request 'V) 200 'OK Response-params Response-body)))))

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
    (prin (build> (http-res (get Request 'V) 200 'OK Response-params Response-body)))))

### start server

(de http-server (Port handler)
  (generic-server Port 'http-reader handler))

(de http-echo-server (Port)
  (http-server Port 'http-handler-echo))
(http-echo-server 8080)
