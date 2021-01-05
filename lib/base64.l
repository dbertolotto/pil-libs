# base64 encoding/decoding
# (C) 2021 Davide Bertolotto
(symbols 'base64 'pico)

(local) *Char64 char64 encode decode

(setq *Char64 '`(chop "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))

(de char64 (A B)
  (default B 0)
  (get *Char64 (inc (| A B))))

(de encode (String)
  (pack
    (pipe
      (prin String)
      (let (B1 (rd 1) B2 (rd 1) B3 (rd 1))
        (make
          (while (or B1 B2 B3)
            (link (char64 (>> 2 B1)))
            (nond
              (B2
                (link
                  (char64 (>> -4 (& B1 3)))
                  '==))
              (B3
                (link
                  (char64 (>> -4 (& B1 3)) (>> 4 B2))
                  (char64 (>> -2 (& B2 15)))
                  '=))
              (NIL
                (link
                  (char64 (>> -4 (& B1 3)) (>> 4 B2))
                  (char64 (>> -2 (& B2 15)) (>> 6 B3))
                  (char64 (& B3 63)))))
            (setq B1 (rd 1) B2 (rd 1) B3 (rd 1))))))))

(de decode (String)
  (let S (chop String)
    (pipe
      (out NIL
        (apply 'wr
               (make
                 (while S
                   (let (A (dec (index (++ S) *Char64))
                         B (dec (index (++ S) *Char64))
                         C (dec (index (++ S) *Char64))
                         D (dec (index (++ S) *Char64)))
                     (link (| (>> -2 A) (>> 4 B)))
                     (and C (link (| (>> -4 (& B 15)) (>> 2 C)))
                          D (link (| (>> -6 (& C 3)) D))))))))
      (till NIL T))))

