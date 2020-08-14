(load+ "#parser/parser.l" "#parser/uri.l")

(def 'bws
  'ows)

(def 'dquote
  (charp "\""))

(def 'vchar
  (arangep 33 126))

# HTTP1.1 RFC7230

(de header-helper (@P)
  (macro
    (seqp (manyp (seqp (charp ",") 'ows)) '@P (manyp (seqp 'ows (charp ",") (optp (seqp 'ows '@P)))))))

(def 'connection
  (header-helper 'connection-option))

(def 'content-length
  (many1p 'digit))

(def 'http-header
  (mapp '((X) (apply 'list X 'header))
    (manyp (droprp 'header-field 'crlf))))

(def 'http-message
  (seqlstp 'start-line
           (droprp 'http-header 'crlf)
           (optp 'message-body)))

(def 'http-name
  (stringp "HTTP"))

(def 'http-version
  (mapp '((X) (cons 'version X))
    (seqlstp (droprp 'http-name (charp "/"))
             (droprp 'digit (charp "."))
             'digit)))

(def 'http-host
  (seqp 'uri-host (optp (seqp (charp ":") 'uri-port))))

(def 'ows
  (manyp 'wsp))

(def 'rws
  (many1p 'wsp))

(def 'te
  (optp (seqp (plusp (charp ",") 't-codings) (manyp (seqp 'ows (charp ",") (optp (seqp 'ows 't-codings)))))))

(def 'trailer
  (header-helper 'field-name))

(def 'transfer-encoding
  (header-helper 'transfer-encoding))

# def URI-reference from uri.l

(def 'upgrade
  (header-helper 'protocol))

(def 'via
  (header-helper (seqp 'received-protocol 'rws 'received-by (optp (seqp 'rws 'comment)))))

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
         (urangep "21" "27")
         (urangep "2A" "5B")
         (urangep "5D" "7E")
         'obs-text))

(def 'field-content
  (seqp 'field-vchar (optp (seqp (many1p (plusp 'sp 'htab)) 'field-vchar))))

(def 'field-name
  'token)

(def 'field-value
  (mapp 'pack
    (manyp (plusp 'field-content 'obs-fold))))

(def 'field-vchar
  (mapp 'pack
    (plusp 'vchar 'obs-text)))

# def fragment from uri.l

(def 'header-field
  (seqp (droprp 'field-name (seqp (charp ":") 'ows))
        (droprp 'field-value 'ows)))

(def 'http-URI
  (seqp (stringp "http://") 'authority 'path-abempty 'query-opt 'fragment-opt))

(def 'https-URI
  (seqp (stringp "https://") 'authority 'path-abempty 'query-opt 'fragment-opt))

(def 'last-chunk
  (seqp (many1p (charp "0")) (optp 'chunk-ext) 'crlf))

(def 'message-body
  (mapp '((X) (list 'body (pack X)))
    (manyp 'octet)))

(def 'http-method
  (mapp '((X) (cons 'method X))
    'token))

#(def 'http-method
#  (eval (cons 'plusp (mapcar 'stringp "GET" "HEAD" "POST" "PUT" "DELETE" "CONNECT" "OPTIONS" "TRACE"))))

(def 'obs-fold
  (seqp 'crlf (many1p 'wsp)))

(def 'obs-text
  (urangep "80" "FF"))

(def 'origin-form
  (seqp 'absolute-path 'query-opt))

(def 'partial-URI
  (seqp 'relative-part 'query-opt))

# def path-abempty from uri.l

# def uri-port from uri.l

(def 'protocol
  (seqp 'protocol-name (optp (droplp (charp "/") 'protocol-version))))

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
         (urangep "23" "5B")
         (urangep "5D" "7E")
         'obs-text))

# def uri-query from uri.l

(def 'quoted-pair
  (seqp (charp "\\") (plusp 'htab 'sp 'vchar 'obs-text)))

(def 'quoted-string
  (betweenp 'dquote (manyp (plusp 'qdtext 'quoted-pair) 'dquote)))

(def 'http-rank
  (plusp (seqp (charp "0") (optp (charp ".") 'digit 'digit 'digit))
         (seqp (charp "1") (optp (charp ".") (charp "0") (charp "0") (charp "0")))))

(def 'reason-phrase
  (mapp '((X) (cons 'reason (pack X)))
    (manyp (plusp 'htab 'sp 'vchar 'obs-text))))

(def 'received-by
  (plusp (seqp 'uri-host (optp (seqp (charp ":" 'uri-port))))
         'pseudonym))

(def 'received-protocol
  (seqp (optp (seqp 'protocol-name (charp "/"))) 'protocol-version))

# def relative-part from uri.l

(def 'request-line
  (seqlstp (droprp 'http-method 'sp)
           (droprp 'request-target 'sp)
           (droprp 'http-version 'crlf)))

(def 'request-target
  (mapp '((X) (cons 'target X))
    (plusp 'origin-form 'absolute-form 'authority-form 'asterisk-form)))

# def scheme from uri.l

# def segment from uri.l

(def 'start-line
  (mapp '((X) (apply 'list X 'start))
    (plusp 'request-line 'status-line)))

(def 'status-code
  (mapp '((X) (cons 'status (format (pack X))))
    (seqp 'digitp 'digitp 'digitp)))

(def 'status-line
  (seqlstp (droprp 'http-version 'sp)
           (droprp 'status-code 'sp)
           (droprp 'reason-phrase 'crlf)))

(def 't-codings
  (plusp (stringp "trailers") (seqp 'transfer-coding (optp 't-ranking))))

(def 't-ranking
  (seqp 'ows (charp ";") 'ows (stringp "q=") 'http-rank))

(def 'tchar
  (plusp (memberp (chop "!#$%&'*+-.\^_|~")) 'digit 'alpha))

(def 'token
  (mapp 'pack
    (many1p 'tchar)))

(def 'trailer-part
  (manyp (droprp 'header-field 'crlf)))

(def 'transfer-coding
  (plusp (stringp "chunked") (stringp "compress") (stringp "deflate") (stringp "gzip") 'transfer-extension))

(def 'transfer-extension
  (seqp 'token (manyp (seqp 'ows (charp ";") 'ows 'transfer-parameter))))

(def 'transfer-parameter
  (seqp 'token 'bws (charp "=") 'bws (plusp 'token 'quoted-string)))

# def uri-host from uri.l

