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

(de parse-params (S)
  (pack
    (make
      (for (L (chop S) L)
        (let (A (pop 'L))
          (link
            (case A
              ("%" (http-char (pop 'L) (pop 'L)))
              ("+" " ")
              (T A))))))))

(bye)
