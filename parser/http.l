(load "parser.l" "uri.l")

(de uni (Str)
  (char (hex Str)))

(def 'bws
  'ows)

(def 'dquote
  (charp "\""))

(def 'vchar
  (satp '((X) (and (>= X `(char 33)) (<= X `(char 126))))))

# HTTP1.1 RFC7230

(de header??? (@P)
  (macro
    (seqp (manyp (seqp (charp ",") 'ows)) '@P (manyp (seqp 'ows (charp ",") (optp (seqp 'ows '@P)))))))

(def 'connection
  (header??? 'connection-option))

(def 'content-length
  (many1p 'digit))

(def 'http-message
  (seqp 'start-line (manyp (seqp 'header-field 'crlf)) 'crlf (optp 'message-body)))

(def 'http-name
  (stringp "HTTP"))

(def 'http-version
  (seqp 'http-name (charp "/") 'digit (charp ".") 'digit))

(def 'http-host
  (seqp 'uri-host (optp (seqp (charp ":") 'uri-port))))

(def 'ows
  (manyp (plusp 'sp 'htab)))

(def 'rws
  (many1p (plusp 'sp 'htab)))

(def 'te
  (optp (seqp (plusp (charp ",") 't-codings) (manyp (seqp 'ows (charp ",") (optp (seqp 'ows 't-codings)))))))

(def 'trailer
  (header??? 'field-name))

(def 'transfer-encoding
  (header??? 'transfer-encoding))

# def URI-reference from uri.l

(def 'upgrade
  (header??? 'protocol))

(def 'via
  (header??? (seqp 'received-protocol 'rws 'received-by (optp (seqp 'rws 'comment)))))

# def absolute-uri from uri.l

(def 'absolute-form
  'absolute-URI)

(def 'absolute-path
  (many1p (seqp (charp "/") 'segment)))

(def 'asterisk-form
  (charp "*"))

# def authority from uri.l

(def 'authority-form
  'authority)

#

(def 'chunk
  (seqp 'chunk-size (optp 'chunk-ext) 'crlf 'chunk-data 'crlf))

(def 'chunk-data
  (many1p 'octet))

(def 'chunk-ext
  (manyp (seqp (charp ";") 'chunk-ext-name (optp (seqp (charp "=") 'chunk-ext-val)))))

(def 'chunk-ext-name
  'token)

(def 'chunk-ext-val
  (plusp 'token 'quoted-string))

(def 'chunk-size
  (many1p 'hexdig))

(def 'chunked-body
  (seqp (manyp 'chunk) 'last-chunk 'trailer-part 'crlf))

(def 'comment
  (seqp (charp "(") (manyp (plusp 'ctext 'quoted-pair 'comment)) (charp ")")))

(def 'connection-option
  'token)

(def 'ctext
  (plusp 'htab
         'sp
         (satp '((X) (and (>= X `(uni "21")) (<= X `(uni "27")))))
         (satp '((X) (and (>= X `(uni "2A")) (<= X `(uni "5B")))))
         (satp '((X) (and (>= X `(uni "5D")) (<= X `(uni "7E")))))
         'obs-text))

(def 'field-content
  (seqp 'field-vchar (optp (seqp (many1p (plusp 'sp 'htab)) 'field-vchar))))

(def 'field-name
  'token)

(def 'field-value
  (manyp (plusp 'field-content 'obs-fold)))

(def 'field-vchar
  (plusp 'vchar 'obs-text))

# def fragment from uri.l

(def 'header-field
  (seqp 'field-name (charp ":") 'ows 'field-value 'ows))

(def 'http-URI
  (seqp (stringp "http://") 'authority 'path-abempty 'query-opt 'fragment-opt))

(def 'https-URI
  (seqp (stringp "https://") 'authority 'path-abempty 'query-opt 'fragment-opt))

(def 'last-chunk
  (seqp (many1p (charp "0")) (optp 'chunk-ext) 'crlf))

(def 'message-body
  (manyp 'octet))

(def 'http-method
  'token)

#(def 'http-method
#  (eval (cons 'plusp (mapcar 'stringp "GET" "HEAD" "POST" "PUT" "DELETE" "CONNECT" "OPTIONS" "TRACE"))))

(def 'obs-fold
  (seqp 'crlf (many1p (plusp 'sp 'htab))))

(def 'obs-text
  (satp '((X) (and (>= X `(uni "80")) (<= X `(uni "FF"))))))

(def 'origin-form
  (seqp 'absolute-path 'query-opt))

(def 'partial-URI
  (seqp 'relative-part 'query-opt))

# def path-abempty from uri.l

# def uri-port from uri.l

(def 'protocol
  (seqp 'protocol-name (optp (seqp (charp "/") 'protocol-version))))

(def 'protocol-name
  'token)

(def 'protocol-version
  'token)

(def 'pseudonym
  'token)

(def 'qdtext
  (plusp 'htab
         'sp
         (charp "!")
         (satp '((X) (and (>= X `(uni "23")) (<= X `(uni "5B")))))
         (satp '((X) (and (>= X `(uni "5D")) (<= X `(uni "7E")))))
         'obs-text))

# def uri-query from uri.l

(def 'quoted-pair
  (seqp (charp "\\") (plusp 'htab 'sp 'vchar 'obs-text)))

(def 'quoted-string
  (seqp 'dquote (manyp (plusp 'qdtext 'quoted-pair) 'dquote)))

(def 'http-rank
  (plusp (seqp (charp "0") (optp (charp ".") 'digit 'digit 'digit))
         (seqp (charp "1") (optp (charp ".") (charp "0") (charp "0") (charp "0")))))

(def 'reason-phrase
  (manyp (plusp 'htab 'sp 'vchar 'obs-text)))

(def 'received-by
  (plusp (seqp 'uri-host (optp (seqp (charp ":" 'uri-port))))
         'pseudonym))

(def 'received-protocol
  (seqp (optp (seqp 'protocol-name (charp "/"))) 'protocol-version))

# def relative-part from uri.l

(def 'request-line
  (seqp 'http-method 'sp 'request-target 'sp 'http-version 'crlf))

(def 'request-target
  (plusp 'origin-form 'absolute-form 'authority-form 'asterisk-form))

# def scheme from uri.l

# def segment from uri.l

(def 'start-line
  (plusp 'request-line 'status-line))

(def 'status-code
  (mapp 'format (mapp 'pack (seqp 'digitp 'digitp 'digitp))))

(def 'status-line
  (seqp 'http-version 'sp 'status-code 'sp 'reason-phrase 'crlf))

(def 't-codings
  (plusp (stringp "trailers") (seqp 'transfer-coding (optp 't-ranking))))

(def 't-ranking
  (seqp 'ows (charp ";") 'ows (stringp "q=") 'http-rank))

(def 'tchar
  (plusp (satp '((X) (member X `(chop "!#$%&'*+-.\^_|~")))) 'digit 'alpha))

(def 'token
  (many1p 'tchar))

(def 'trailer-part
  (manyp (seqp 'header-field 'crlf)))

(def 'transfer-coding
  (plusp (stringp "chunked") (stringp "compress") (stringp "deflate") (stringp "gzip") 'transfer-extension))

(def 'transfer-extension
  (seqp 'token (manyp (seqp 'ows (charp ";") 'ows 'transfer-parameter))))

(def 'transfer-parameter
  (seqp 'token 'bws (charp "=") 'bws (plusp 'token 'quoted-string)))

# def uri-host from uri.l

