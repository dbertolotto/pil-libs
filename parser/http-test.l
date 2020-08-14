(load "http.l")

(println (parse start-line "GET /hello.txt HTTP/1.1\r"))
(println (parse request-line "GET /hello.txt HTTP/1.1\r"))
# origin form
(println (parse start-line "GET /where?q=now HTTP/1.1\r"))
(println (parse start-line "GET /where?q=now#bla HTTP/1.1\r"))
(println (parse start-line "GET /where#bla HTTP/1.1\r"))
# absolute form
(println (parse start-line "GET http://www.example.org/pub/WWW/TheProject.html HTTP/1.1\r"))
# authority form
(println (parse start-line "CONNECT www.example.com:80 HTTP/1.1\r"))
# asterisk form
(println (parse start-line "OPTIONS * HTTP/1.1\r"))


(println (parse http-message
"GET /hello.txt HTTP/1.1\r
User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r
Host: www.example.com\r
Accept-Language: en, mi\r
\r
"))
(println (parse http-message "HTTP/1.1 200 OK\r
Date: Mon, 27 Jul 2009 12:28:53 GMT\r
Server: Apache\r
Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT\r
ETag: \"34aa387-d-1568eb00\"\r
Accept-Ranges: bytes\r
Content-Length: 51\r
Vary: Accept-Encoding\r
Content-Type: text/plain\r
\r
Hello World! My payload includes a trailing CRLF.\r
"))
