(symbols 'string 'pico)

(local) triml
(de triml (S)
  (pack (flip (trim (flip (chop S))))))

(local) trimr
(de trimr (S)
  (pack (trim (chop S))))

(local) trims
(de trims (S)
  (pack (clip (chop S))))

(local) splits
(de splits (S Separator)
  (mapcar 'pack (split (chop S) Separator)))
