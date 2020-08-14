(load+ "#parser/parser.l")

# URI RFC3986

(def 'query-opt
  (mapp '((X) (cons 'query X))
    (optp (droplp (charp "?") 'uri-query))))

(def 'fragment-opt
  (mapp '((X) (cons 'fragment X))
    (optp (droplp (charp "#") 'uri-fragment))))

(def 'URI # complete URI
  (seqlstp (droprp 'scheme (charp ":")) 'hier-part 'query-opt 'fragment-opt))

(def 'hier-part
  (mapp '((X) (cons 'hier X))
  (plusp (seqlstp (droplp (stringp "//") 'authority) 'path-abempty)
         'path-absolute
         'path-rootless
         'path-empty)))

(def 'URI-reference
  (plusp 'URI 'relative-ref))

(def 'absolute-URI # no fragment
  (seqlstp (droprp 'scheme (charp ":")) 'hier-part 'query-opt))

(def 'relative-ref
  (seqlstp 'relative-part 'query-opt 'fragment-opt))

(def 'relative-part
  (mapp '((X) (cons 'hier X))
  (plusp (seqlstp (droplp (stringp "//") 'authority) 'path-abempty)
         'path-absolute
         'path-noscheme
         'path-empty)))

(def 'scheme
  (mapp '((X) (cons 'scheme X))
    (mapp 'pack
      (seqp 'alpha (manyp 'alpha 'digit (charp "+") (charp "-") (charp "."))))))

(def 'authority
  (mapp '((X) (cons 'authority X))
    (seqlstp (mapp '((X) (cons 'userinfo X))
                   (optp (droprp 'userinfo (charp "@"))))
             (mapp '((X) (cons 'host X))
                   'uri-host)
             (mapp '((X) (cons 'port X))
                   (optp (droplp (charp ":") 'uri-port))))))

(def 'userinfo
  (mapp 'pack
    (manyp (plusp 'unreserved 'pct-encoded 'sub-delims (charp ":")))))

(def 'uri-host
  (plusp 'IP-literal 'IPv4address 'reg-name))

(def 'uri-port
  (mapp 'format
    (mapp 'pack
      (manyp 'digit))))

(def 'IP-literal # RFC6874
  (betweenp (charp "[") (plusp 'IPv6addrz 'IPvFuture) (charp "]")))

(def 'IPv6addrz # RFC6874
  (seqlstp (mapp '((X) (cons 'ipv6 X))
             'IPv6address)
           (mapp '((X) (cons 'zone X))
             (optp (droplp (stringp "%25") 'zone-id)))))

(def 'zone-id # RFC6874
  (mapp 'pack
    (many1p (plusp 'unreserved 'pct-encoded))))

(def 'IPvFuture
  (mapp '((X) (cons 'ipvf X))
    (mapp 'pack
      (seqp (charp "v") (many1p 'hexdig) (charp ".") (many1p (plusp 'unreserved 'sub-delims (charp ":")))))))

(def 'h16
  (mapp 'pack
    (manyxyp 'hexdig 1 4)))

(def 'h16:
  (droprp 'h16 (charp ":")))

(def 'ls32
  (plusp (seqp 'h16: 'h16) IPv4address))

(def 'IPv6address # this had to be splitted in order to avoid ambiguities
  (let dcolon (stringp "::")
    (mapp '((X) (fish 'atom X))
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
        (seqp (optp (seqp (manyxyp 'h16: 6 6) 'h16)) dcolon)
        ))))

(def 'IPv4address
  (let dot-dec-octet (droplp (charp ".") 'dec-octet)
    (mapp '((X) (cons 'ipv4 X))
      (mapp '((X) (fish 'atom X))
        (seqp 'dec-octet dot-dec-octet dot-dec-octet dot-dec-octet)))))

(def 'dec-octet
  (let uni '((Str) (char (hex Str)))
    (mapp 'format
      (mapp 'pack
        (plusp # order is relevant here!
          (seqp (stringp "25") (crangep "0" "5")) # 250-255
          (seqp (charp "2") (crangep "0" "4") 'digit) # 200-249
          (seqp (charp "1") 'digit 'digit) # 100-199
          (seqp (crangep "1" "9") 'digit) # 10-99
          'digit # 0-9
          )))))

(def 'reg-name
  (mapp '((X) (cons 'name X))
    (mapp 'pack
      (manyp (plusp 'unreserved 'pct-encoded 'sub-delims)))))

(def 'pathp # path parser
  (plusp 'path-abempty 'path-absolute 'path-noscheme 'path-rootless 'path-empty))

(def 'path-abempty # begins with "/" or is empty
  (mapp '((X) (cons 'path X))
    (manyp (droplp (charp "/") 'segment))))

(def 'path-absolute # begins with "/" but not "//"
  (mapp '((X) (cons 'path X))
    (seqp (charp "/") (optp (seqp 'segment-nz (manyp (droplp (charp "/") 'segment)))))))

(def 'path-noscheme # begins with a non-colon segment
  (mapp '((X) (cons 'path X))
    (seqp 'segment-nz-nc (manyp (droplp (charp "/") 'segment)))))

(def 'path-rootless # begins with a segment
  (mapp '((X) (cons 'path X))
    (seqp 'segment-nz (manyp (droplp (charp "/") 'segment)))))

(def 'path-empty # zero characters
  (mapp '((X) (cons 'path X))
    'nilp))

(def 'segment
  (mapp 'pack
    (manyp 'pchar)))

(def 'segment-nz
  (mapp 'pack
    (many1p 'pchar)))

(def 'segment-nz-nc
  (mapp 'pack
    (many1p (plusp 'unreserved 'pct-encoded 'sub-delims (charp "@")))))

(def 'pchar
  (plusp 'unreserved 'pct-encoded 'sub-delims (charp ":") (charp "@")))

(def 'uri-query
  (mapp 'pack
    (manyp (plusp 'pchar (charp "/") (charp "?")))))

(def 'uri-fragment
  (mapp 'pack
    (manyp (plusp 'pchar (charp "/") (charp "?")))))

(def 'pct-encoded
  (seqp (charp "%") 'hexdig 'hexdig))

(def 'unreserved
  (plusp 'alpha 'digit (charp "-") (charp ".") (charp "_") (charp "~")))

(def 'reserved
  (plusp gen-delims 'sub-delims))

(def 'gen-delims
  (memberp (chop ":/?#[]@")))

(def 'sub-delims
  (memberp (chop "!$&'()*+,;=")))

