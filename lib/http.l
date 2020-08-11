(setq *HttpNewLine "\r\n")

(class +HttpUri)
# Path, Query, Fragment

(class +HttpReq)
# Method, Uri, Version, Headers, Body
(dm build> ()
  (pack
    (: Method) " " (: Uri) " " (: Version) *HttpNewLine
    (http-headers (: Headers) *HttpNewLine)
    (content-length (: Body))
    *HttpNewLine
    (: Body)))

(de http-req (Meth Uri Ver Head Body)
  (let Req (new '(+HttpReq))
    (put Req 'Method Meth)
    (put Req 'Uri Uri)
    (put Req 'Version Ver)
    (put Req 'Headers Head)
    (put Req 'Body Body)))

(class +HttpRes)
# Version, Status, Message, Headers, Body
(dm build> ()
  (pack
    (: Version) " " (: Status) " " (: Message) *HttpNewLine
    (http-headers (: Headers))
    (content-length (: Body))
    *HttpNewLine
    (: Body)))

(de http-headers (Headers)
  (mapcar '((H) (pack (car H) ": " (cdr H) NewLine)) Headers))

(de content-length (Body)
  (when Body
    (pack "content-length: " (length Body) *HttpNewLine)))
