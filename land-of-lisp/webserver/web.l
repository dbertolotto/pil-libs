(de http-char (C1 C2)
  (char (hex (pack C1 C2))))

(test "?" (http-char "3" "f"))

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

(de parse-params (S)
  (mapcar 'parse-param (ssplit S "&")))

(test '((aa . "bb")) (parse-params "aa=bb"))
(test '((aa . "bb") (cc . NIL)) (parse-params "aa=bb&cc="))
(test '((aa . "bb") (cc . "dd")) (parse-params "aa=bb&cc=dd"))

(de nil? (E)
  (== E NIL))

(de nnil? (E)
  (not (nil? E)))

(test T (nnil? 'a))
(test NIL (nnil? NIL))

(de parse-url (S)
  (let (L (ssplit S " ")
        Lf (filter nnil? L)
        Url (ssplit (cadr Lf) "?"))
    (list (intern (car Lf))
          (car Url)
          (parse-params (cadr Url))
          (caddr Lf))))

(test '(GET "/lolcats.html" NIL "HTTP/1.1") (parse-url "GET  /lolcats.html  HTTP/1.1"))
(test '(GET "/lolcats.html" ((extra-funny . "yes")) "HTTP/1.1")
      (parse-url "GET  /lolcats.html?extra-funny=yes  HTTP/1.1"))

(de triml (S)
  (pack (flip (trim (flip (chop S))))))

(de trimr (S)
  (pack (trim (chop S))))

(de trimlr (S)
  (pack (clip (chop S))))

(test "a b" (trimlr " a b "))

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

(de get-content-param (Header)
  (let (Length (format (cdar (filter '((P) (= (car P) 'content-length)) Header))))
    (when Length
      (parse-params (pack (head Length (till NIL)))))))

(test '((aa . "bb") (cc . "dd"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 11^J^Jaa=bb&cc=dd")
            (get-content-param (get-header))))
(test '((aa . "bb") (cc . "d"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 10^J^Jaa=bb&cc=dd")
            (get-content-param (get-header))))
(test '((aa . "bb") (cc . "dd^J"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 12^J^Jaa=bb&cc=dd")
            (get-content-param (get-header))))


(server (Request-handler PortN)
  (loop
    (setq Port (port PortN)
    (setq Sock (listen Port)) # Listen
    (prinl "Server (Pid " *Pid ") listening on port "     PortN)
    (NIL (fork) (child-handler Request-handler Port)) # Accepted
    (close Sock)) # Parent: Close socket and continue

(de child-handler (Request-handler Port)
   (close Port)                                         (prinl (stamp) " | " *Pid " | Client connected from "
  *Adr)
   (in Sock
     (until (eof)
       (out Sock
         (Request-handler))
       (close Sock)))
   (prinl (stamp) " | " *Pid " | Client disconnected")
   (bye))

#{
(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
       (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url    (parse-url (read-line stream)))
                        (path   (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
       (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<form>What is your name?<input name='name' /></form>")
            (format t "Nice to meet you, ~a!" (cdr name))))
      (princ "Sorry... I don't know that page.")))
}#

(bye)
