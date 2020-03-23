(symbols 'string 'pico)

# string trimming functions

(local) triml
(de triml (S)
  (pack (flip (trim (flip (chop S))))))
(test "a b " (triml " a b "))

(local) trimr
(de trimr (S)
  (pack (trim (chop S))))
(test " a b" (trimr " a b "))

(local) trimlr
(de trimlr (S)
  (pack (clip (chop S))))
(test "a b" (trimlr " a b "))
