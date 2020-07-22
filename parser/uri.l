(load "parser.l")

# URI RFC3986

(def 'query-opt
  (optp (seqp (charp "?") 'uri-query)))

(def 'fragment-opt
  (optp (seqp (charp "#") 'fragment)))

(def 'URI
  (seqp 'scheme (charp ":") 'hier-part 'query-opt 'fragment-opt))

(def 'hier-part
  (plusp (seqp (stringp "//") 'authority 'path-abempty) 'path-absolute 'path-rootless 'path-empty))

(def 'URI-reference
  (plusp 'URI 'relative-ref))

(def 'absolute-URI
  (seqp 'scheme (charp ":") 'hier-part 'query-opt))

(def 'relative-ref
  (seqp 'relative-part 'query-opt 'fragment-opt))

(def 'relative-part
  (plusp (seqp (stringp "//") 'authority 'path-abempty) 'path-absolute 'path-noscheme 'path-empty))

(def 'scheme
  (mapp 'pack (seqp 'alpha (manyp 'alpha 'digit (charp "+") (charp "-") (charp ".")))))

(def 'authority
  (seqp (optp (seqp 'userinfo (charp "@"))) 'uri-host (optp (seqp (charp ":") 'uri-port))))

(def 'userinfo
  (mapp 'pack (manyp (plusp 'unreserved 'pct-encoded 'sub-delims (charp ":")))))

(def 'uri-host
  (plusp 'IP-literal 'IPv4address 'reg-name))

(def 'uri-port
  (mapp 'format (mapp 'pack (manyp 'digit))))

(def 'IP-literal # RFC6874
  (seqp (charp "[") (plusp 'IPv6address 'IPv6addrz 'IPvFuture) (charp "]")))

(def 'IPv6addrz # RFC6874
  (seqp 'IPv6address (stringp "%25") 'zone-id))

(def 'zone-id # RFC6874
  (mapp 'pack (many1p (plusp 'unreserved 'pct-encoded))))

(def 'IPvFuture
  (seqp (charp "v") (many1p 'hexdig) (charp ".") (many1p (plusp 'unreserved 'sub-delims (charp ":")))))

(def 'h16
  (mapp 'pack (manyxyp 'hexdig 1 4)))

(def 'h16:
  (seqp 'h16 (charp ":")))

(def 'ls32
  (plusp (seqp 'h16 (charp ":") 'h16) IPv4adress))

(def 'IPv6address
  (let dcolon (stringp "::")
    (plusp
      (seqp                                               (manyxyp 'h16: 6 6) 'ls32)
      (seqp                                        dcolon (manyxyp 'h16: 5 5) 'ls32)
      (seqp (optp                           'h16)  dcolon (manyxyp 'h16: 4 4) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 0 1) 'h16)) dcolon (manyxyp 'h16: 3 3) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 0 2) 'h16)) dcolon (manyxyp 'h16: 2 2) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 0 3) 'h16)) dcolon (manyxyp 'h16: 1 1) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 0 4) 'h16)) dcolon                     'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 0 5) 'h16)) dcolon                     'h16)
      (seqp (optp (seqp (manyxyp 'h16: 0 6) 'h16)) dcolon))))

(def 'IPv6address # this had to be splitted in order to avoid ambiguities
  (let dcolon (stringp "::")
    (plusp
      (seqp                                               (manyxyp 'h16: 6 6) 'ls32)
      (seqp                                        dcolon (manyxyp 'h16: 5 5) 'ls32)
      (seqp (optp                           'h16)  dcolon (manyxyp 'h16: 4 4) 'ls32)
      # split
      (seqp (optp                           'h16)  dcolon (manyxyp 'h16: 3 3) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 1 1) 'h16)) dcolon (manyxyp 'h16: 3 3) 'ls32)
      # split
      (seqp (optp                           'h16)  dcolon (manyxyp 'h16: 2 2) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 1 1) 'h16)) dcolon (manyxyp 'h16: 2 2) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 2 2) 'h16)) dcolon (manyxyp 'h16: 2 2) 'ls32)
      # split
      (seqp (optp                           'h16)  dcolon (manyxyp 'h16: 1 1) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 1 1) 'h16)) dcolon (manyxyp 'h16: 1 1) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 2 2) 'h16)) dcolon (manyxyp 'h16: 1 1) 'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 3 3) 'h16)) dcolon (manyxyp 'h16: 1 1) 'ls32)
      # split
      (seqp (optp                           'h16)  dcolon                     'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 1 1) 'h16)) dcolon                     'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 2 2) 'h16)) dcolon                     'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 3 3) 'h16)) dcolon                     'ls32)
      (seqp (optp (seqp (manyxyp 'h16: 4 4) 'h16)) dcolon                     'ls32)
      # split
      (seqp (optp                           'h16)  dcolon                     'h16)
      (seqp (optp (seqp (manyxyp 'h16: 1 1) 'h16)) dcolon                     'h16)
      (seqp (optp (seqp (manyxyp 'h16: 2 2) 'h16)) dcolon                     'h16)
      (seqp (optp (seqp (manyxyp 'h16: 3 3) 'h16)) dcolon                     'h16)
      (seqp (optp (seqp (manyxyp 'h16: 4 4) 'h16)) dcolon                     'h16)
      (seqp (optp (seqp (manyxyp 'h16: 5 5) 'h16)) dcolon                     'h16)
      # split
      (seqp (optp                           'h16)  dcolon)
      (seqp (optp (seqp (manyxyp 'h16: 1 1) 'h16)) dcolon)
      (seqp (optp (seqp (manyxyp 'h16: 2 2) 'h16)) dcolon)
      (seqp (optp (seqp (manyxyp 'h16: 3 3) 'h16)) dcolon)
      (seqp (optp (seqp (manyxyp 'h16: 4 4) 'h16)) dcolon)
      (seqp (optp (seqp (manyxyp 'h16: 5 5) 'h16)) dcolon)
      (seqp (optp (seqp (manyxyp 'h16: 6 6) 'h16)) dcolon))))

(def 'IPv4address
  (seqp 'dec-octet (charp ".") 'dec-octet (charp ".") 'dec-octet (charp ".") 'dec-octet))

(def 'dec-octet
  (let uni '((Str) (char (hex Str)))
    (mapp 'pack
      (plusp
        'digit
        (seqp (satp '((X) (and (>= X `(uni "31")) (<= X `(uni "39"))))) 'digit)
        (seqp (charp "1") 'digit 'digit)
        (seqp (charp "2") (satp '((X) (and (>= X `(uni "30")) (<= X `(uni "34"))))) 'digit)
        (seqp (stringp "25") (satp '((X) (and (>= X `(uni "30")) (<= X `(uni "35"))))))))))

(def 'reg-name
  (mapp 'pack (manyp (plusp 'unreserved 'pct-encoded 'sub-delims))))

(def 'pathp
  (plusp 'path-abempty 'path-absolute 'path-noscheme 'path-rootless 'path-empty))

(def 'path-abempty
  (manyp (seqp (charp "/") 'segment)))

(def 'path-absolute
  (seqp (charp "/" (optp (seqp 'segment-nz 'path-abempty)))))

(def 'path-noscheme
  (seqp 'segment-nz-nc 'path-abempty))

(def 'path-rootless
  (seqp 'segment-nz 'path-abempty))

(def 'path-empty
  (resultp ""))

(def 'segment
  (mapp 'pack (manyp 'pchar)))

(def 'segment-nz
  (mapp 'pack (many1p 'pchar)))

(def 'segment-nz-nc
  (mapp 'pack (many1p (plusp 'unreserved 'pct-encoded 'sub-delims (charp "@")))))

(def 'pchar
  (plusp 'unreserved 'pct-encoded 'sub-delims (charp ":") (charp "@")))

(def 'uri-query
  (mapp 'pack (manyp (plusp 'pchar (charp "/") (charp "?")))))

(def 'fragment
  (mapp 'pack (manyp (plusp 'pchar (charp "/") (charp "?")))))

(def 'pct-encoded
  (seqp (charp "%") 'hexdig 'hexdig))

(def 'unreserved
  (plusp 'alpha 'digit (charp "-") (charp ".") (charp "_") (charp "~")))

(def 'reserved
  (plusp gen-delims 'sub-delims))

(def 'gen-delims
  (satp '((X) (member X '`(chop ":/?#[]@")))))

(def 'sub-delims
  (satp '((X) (member X '`(chop "!$&'()*+,;=")))))

