(symbols 'http 'pico)

(local) *HttpNeeLine
(setq *HttpNewLine "\r\n")

(local) +HttpUri
(class +HttpUri)
(dm T (Path Query Fragment)
  (=: P Path)
  (=: Q Query)
  (=: F Fragment))
(dm params> ()
  (parse-params (: Q)))
(dm build> ()
  (pack (: P) (when (: Q) (pack "?" @)) (when (: F) (pack "#" @))))

(de http-uri @
  (pass new '(+HttpUri)))

(local) +HttReq
(class +HttpReq)
(dm T (Method Uri Version Headers Body)
  (=: M Method)
  (=: U Uri)
  (=: V Version)
  (=: H Headers)
  (=: B Body))
(dm build> ()
  (pack
    (: M) " " (build> (: U)) " " (: V) *HttpNewLine
    (build-http-headers (: H))
    (content-length (: B))
    *HttpNewLine
    (: B)))

(de http-req @
  (pass new '(+HttpReq)))

(local) +HttpRes
(class +HttpRes)
(dm T (Version Status Message Headers Body)
  (=: V Version)
  (=: S Status)
  (=: M Message)
  (=: H Headers)
  (=: B Body))
(dm build> ()
  (pack
    (: V) " " (: S) " " (: M) *HttpNewLine
    (build-http-headers (: H))
    (content-length (: B))
    *HttpNewLine
    (: B)))

(de http-res @
  (pass new '(+HttpRes)))

(de build-http-headers (Headers)
  (mapcar '((H) (pack (car H) ": " (cdr H) *HttpNewLine)) Headers))

(de content-length (Body)
  (when Body
    (pack "content-length: " (length Body) *HttpNewLine)))

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

(de splits (Str Sep)
  (mapcar 'pack (split (chop Str) Sep)))
