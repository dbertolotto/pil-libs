(setq *HttpNewLine "\r\n")

(class +HttpUri)
# Path, Query, Fragment
(dm params> ()
  (parse-params (: Query)))
(dm build> ()
  (pack (: Path) (when (: Query) (pack "?" @)) (when (: Frag) (pack "#" @))))

(de http-uri (Path Query Frag)
  (let Uri (new '(+HttpUri))
    (put Uri 'Path Path)
    (put Uri 'Query Query)
    (put Uri 'Frag Frag)
    Uri))

(class +HttpReq)
# Method, Uri, Version, Headers, Body
(dm build> ()
  (pack
    (: Method) " " (build> (: Uri)) " " (: Version) *HttpNewLine
    (build-http-headers (: Headers))
    (content-length (: Body))
    *HttpNewLine
    (: Body)))

(de http-req (Meth Uri Ver Head Body)
  (let Req (new '(+HttpReq))
    (put Req 'Method Meth)
    (put Req 'Uri Uri)
    (put Req 'Version Ver)
    (put Req 'Headers Head)
    (put Req 'Body Body)
    Req))

(class +HttpRes)
# Version, Status, Message, Headers, Body
(dm build> ()
  (pack
    (: Version) " " (: Status) " " (: Message) *HttpNewLine
    (build-http-headers (: Headers))
    (content-length (: Body))
    *HttpNewLine
    (: Body)))

(de http-res (Ver Stat Msg Head Body)
  (let Res (new '(+HttpRes))
    (put Res 'Version Ver)
    (put Res 'Status Stat)
    (put Res 'Message Msg)
    (put Res 'Headers Head)
    (put Res 'Body Body)
    Res))

(de build-http-headers (Headers)
  (mapcar '((H) (pack (car H) ": " (cdr H) *HttpNewLine)) Headers))

(de content-length (Body)
  (when Body
    (pack "content-length: " (length Body) *HttpNewLine)))

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

(de splits (Str Sep)
  (mapcar 'pack (split (chop Str) Sep)))

(de nnil? (X)
  (<> X NIL))
