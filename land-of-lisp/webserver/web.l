(load "../../lib/string.l" "../../lib/util.l")
(symbols '(pico string util))

(de http-char (C1 C2)
  (char (hex (pack C1 C2))))

(test "?" (http-char "3" "f"))
(test " " (http-char "2" "0"))

(de decode-param (S)
  (pack
    (make
      (for (L (chop S) L)
        (let (A (pop 'L))
          (link
            (case A
              ("%" (http-char (pop 'L) (pop 'L)))
              ("+" " ")
              (T A))))))))

(test "" (decode-param ""))
(test "f" (decode-param "f"))
(test "foo" (decode-param "foo"))
(test "?" (decode-param "%3f"))
(test "foo?" (decode-param "foo%3f"))
(test "?baz" (decode-param "%3fbaz"))
(test "foo?baz" (decode-param "foo%3fbaz"))
(test " " (decode-param "%20"))
(test " " (decode-param "+"))
(test " baz" (decode-param "+baz"))
(test "foo " (decode-param "foo+"))
(test "foo baz" (decode-param "foo+baz"))

(de ssplit (S SS)
  (mapcar 'pack (split (chop S) SS)))

(test NIL (ssplit NIL NIL))
(test NIL (ssplit NIL "="))
(test NIL (ssplit "" "="))
(test '("aa") (ssplit "aa" NIL))
(test '("aa") (ssplit "aa" ""))
(test '("aa") (ssplit "aa" "="))
(test '("aa" NIL) (ssplit "aa=" "="))
(test '(NIL "aa") (ssplit "=aa" "="))
(test '("aa" NIL NIL) (ssplit "aa==" "="))
(test '("aa" "bb") (ssplit "aa=bb" "="))
(test '("aa" "bb" "cc") (ssplit "aa=bb=cc" "="))

(de parse-param (S)
  (let (L (ssplit S "="))
    (cons (intern (car L))
          (decode-param (cadr L)))))

(test '(aa . NIL) (parse-param "aa"))
(test '(aa . NIL) (parse-param "aa="))
(test '(NIL . "bb") (parse-param "=bb"))
(test '(aa . "bb") (parse-param "aa=bb"))
(test '(aa . "über") (parse-param "aa=%FCber"))

(de parse-params (S)
  (mapcar 'parse-param (ssplit S "&")))

(test '((aa . "bb")) (parse-params "aa=bb"))
(test '((aa . "bb") (cc . NIL)) (parse-params "aa=bb&cc="))
(test '((aa . "bb") (cc . "dd")) (parse-params "aa=bb&cc=dd"))

(de parse-url (S)
  (let (L (ssplit S " ")
        Lf (filter nnil? L)
        Url (ssplit (cadr Lf) "?"))
    (list (intern (car Lf))
          (car Url)
          (parse-params (cadr Url))
          (caddr Lf))))

(test '(GET "/" NIL "HTTP/1.1") (parse-url "GET  /  HTTP/1.1"))
(test '(GET "/lolcats.html" NIL "HTTP/1.1") (parse-url "GET  /lolcats.html  HTTP/1.1"))
(test '(GET "/lolcats.html" ((extra-funny . "yes")) "HTTP/1.1")
      (parse-url "GET  /lolcats.html?extra-funny=yes  HTTP/1.1"))

#(de process-header-line (Line)
#  (let))

(de get-header ()
  (make
    (while (line T)
      (let (L (mapcar 'trimlr (ssplit @ ":")))
        (when L
          (link (cons (intern (car L)) (cadr L))))))))

(test '((foo . "1") (bar . "abc, 123"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^J^Jbody:something") (get-header)))

(de get-content-params (Header)
  (let (Length (format (cdar (filter '((P) (= (car P) 'content-length)) Header))))
    (when Length
      (parse-params (pack (head Length (till NIL)))))))

(test '((aa . "bb") (cc . "dd"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 11^J^Jaa=bb&cc=dd")
            (get-content-params (get-header))))
(test '((aa . "bb") (cc . "d"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 10^J^Jaa=bb&cc=dd")
            (get-content-params (get-header))))
(test '((aa . "bb") (cc . "dd^J"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 12^J^Jaa=bb&cc=dd")
            (get-content-params (get-header))))

(de server (Request-handler PortN)
  (let (Port (port PortN))
    (prinl (stamp) " | " *Pid " | " "Server listening on port " PortN)
    (loop
      (setq Sock (listen Port)) # Listen
      (NIL (fork) (child-handler Request-handler Port)) # Accepted
      (close Sock)))) # Parent: Close socket and continue

(de child-handler (Request-handler Port)
   (close Port)
   (prinl (stamp) " | " *Pid " | Client connected from " *Adr)
   (in Sock
     (until (eof)
       (let (Url-line (line T)
             Url (parse-url Url-line)
             Type (car Url)
             Path (cadr Url)
             Url-params (caddr Url)
             Version (cadddr Url)
             Header (get-header)
             Params (append Url-params (get-content-params Header))
             Response-body (Request-handler Type Path Header Params)
             Response (response Version 200 'OK '((content-type . "text/html; charset=utf-8")) Response-body))
         (prinl (stamp) " | " *Pid " | URL: " Url-line)
         (prinl (stamp) " | " *Pid " | Type: " Type)
         (prinl (stamp) " | " *Pid " | Path: " Path)
         (prinl (stamp) " | " *Pid " | URL-params: " Url-params)
         (prinl (stamp) " | " *Pid " | Version: " Version)
         (for P Header
           (prinl (stamp) " | " *Pid " | Header: " (car P) ": " (cdr P)))
         (for P Params
           (prinl (stamp) " | " *Pid " | Params: " (car P) ": " (cdr P)))
         (prinl Response-Body)
         (prinl Response)
         (out Sock
           (prinl Response)))
       (close Sock)))
   (prinl (stamp) " | " *Pid " | Client disconnected")
   (bye))

(de hello-request-handler (Type Path Header Params)
  (if (= Path "/greeting")
    (let (Name (assoc 'name Params))
      (if Name
        (text "<html>Nice to meet you, @1!</html>" (cdr Name))
        "<form>What is your name?<input name='name' /></form>"))
    "<html>Sorry... I don't know that page.</html>"))

(test "<html>Sorry... I don't know that page.</html>"
      (hello-request-handler 'GET "/lolcats" NIL NIL))
(test "<form>What is your name?<input name='name' /></form>"
      (hello-request-handler 'GET "/greeting" NIL NIL))
(test "<html>Nice to meet you, Bob!</html>" 
      (hello-request-handler 'GET "/greeting" NIL '((name . "Bob"))))

(de response (Version Status Message Header Body)
  (pack Version " " Status " " Message "\n"
        (mapcar '((P) (pack (car P) ": " (cdr P) "\n")) Header)
        "\n"
        Body))

(server hello-request-handler 8080)
(bye)
