(symbols 'crypto 'pico)

(de md5 (Str)
  (pack
    (mapcar '((B) (pad 2 (hex B)))
            (native "libcrypto.so" "MD5" '(B . 16) Str (length Str) '(NIL (16))))))

