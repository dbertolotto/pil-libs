(symbols 'io 'pico)

(de read-file (Path)
  (in Path (till NIL T)))

(de write-file (Path Str)
  (out Path (prin Str)))

