(load "web.l")
(symbols '(pico web))

(de test-decode-char ()
  (test "?" (decode-char "3" "f"))
  (test " " (decode-char "2" "0")))

(de test-decode-param ()
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
  (test "foo baz" (decode-param "foo+baz")))

(de test-parse-param ()
  (test '(aa . NIL) (parse-param "aa"))
  (test '(aa . NIL) (parse-param "aa="))
  (test '(NIL . "bb") (parse-param "=bb"))
  (test '(aa . "bb") (parse-param "aa=bb"))
  (test '(aa . "über") (parse-param "aa=%FCber")))

(de test-parse-params ()
  (test '((aa . "bb")) (parse-params "aa=bb"))
  (test '((aa . "bb") (cc . NIL)) (parse-params "aa=bb&cc="))
  (test '((aa . "bb") (cc . "dd")) (parse-params "aa=bb&cc=dd")))

(de test-parse-url ()
  (test '(GET "/" NIL "HTTP/1.1")
        (parse-url "GET  /  HTTP/1.1"))
  (test '(GET "/lolcats.html" NIL "HTTP/1.1")
        (parse-url "GET  /lolcats.html  HTTP/1.1"))
  (test '(GET "/lolcats.html" ((extra-funny . "yes")) "HTTP/1.1")
        (parse-url "GET  /lolcats.html?extra-funny=yes  HTTP/1.1")))

(de test-get-header ()
  (test '((foo . "1") (bar . "abc, 123"))
      (pipe (prinl "foo: 1^Jbar: abc, 123^J^Jbody:something") (get-header))))

(de test-get-content-params ()
  (test '((aa . "bb") (cc . "dd"))
        (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 11^J^Jaa=bb&cc=dd")
              (get-content-params (get-header))))
  (test '((aa . "bb") (cc . "d"))
        (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 10^J^Jaa=bb&cc=dd")
              (get-content-params (get-header))))
  (test '((aa . "bb") (cc . "dd^J"))
        (pipe (prinl "foo: 1^Jbar: abc, 123^Jcontent-length: 12^J^Jaa=bb&cc=dd")
              (get-content-params (get-header)))))

(de test-hello-request-handler ()
  (test "<html>Sorry... I don't know that page.</html>"
        (hello-request-handler 'GET "/lolcats" NIL NIL))
  (test "<form>What is your name?<input name='name' /></form>"
        (hello-request-handler 'GET "/greeting" NIL NIL))
  (test "<html>Nice to meet you, Bob!</html>"
        (hello-request-handler 'GET "/greeting" NIL '((name . "Bob")))))

(de tests ()
  (test-decode-char)
  (test-decode-param)
  (test-parse-param)
  (test-parse-params)
  (test-parse-url)
  (test-get-header)
  (test-get-content-params)
  (test-hello-request-handler))