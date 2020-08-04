(setq *Base64 '`(chop "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))

(def '*Base64Enc
  (make
    (for (I . C) *Base64
      (link (cons (pad 6 (bin (dec I))) C)))))

(def '*Base64Dec
  (mapcar '((M) (cons (cdr M) (car M))) *Base64Enc))

(de get-char (Sext)
  (cdr (assoc Sext *Base64Enc)))

(de get-bit (Char)
  (cdr (assoc Char *Base64Dec)))

(de octets (String)
  (pipe (prin String)
    (make
      (while (rd 1)
        (link (pad 8 (bin @)))))))

(de sextets (String)
  (let Bits (chop String)
    (make
      (while Bits
        (link (pack (cut 6 'Bits)))))))

(de padding (Bytes)
  (let Rem (% (length Bytes) 6)
    (case Rem
      (0 '("" . ""))
      (4 '("=" . "00"))
      (2 '("==" . "0000")))))

(de base64e (String)
  (let (Bytes (pack (octets String))
        (StrPad . BitPad) (padding Bytes)
        Sexs (sextets (pack Bytes BitPad)))
    (pack (mapcar 'get-char Sexs) StrPad)))

# rosetta
(de char64 (A B)
   (default B 0)
  (get *Base64 (inc (| A B))))

(de base64-ros (S)
  (pack (pipe (prin S)
    (let (A (rd 1) B (rd 1) C (rd 1))
      (make
        (while (or A B C)
          (link (char64 (>> 2 A)))
          (nond
            (B
              (link
                (char64 (>> -4 (& A 3)))
                '=
                '=))
            (C
              (link                                                    (char64 (>> -4 (& A 3)) (>> 4 B))
                (char64 (>> -2 (& B 15)))
                '=))
            (NIL
              (link
                (char64 (>> -4 (& A 3)) (>> 4 B))
                (char64 (>> -2 (& B 15)) (>> 6 C))
                (char64 (& C 63)))))
          (setq A (rd 1) B (rd 1) C (rd 1))))))))

(setq test-str "Man is distinguished, not only by his reason, but by this singular passion from other animals,
which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable
generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

(setq test-b64 "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLAp3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlCmdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")

(test test-b64 (base64e test-str))
(test test-b64 (base64-ros test-str))

(test "4a2W" (base64e (char 6998)))
(test "4a2W" (base64-ros (char 6998)))

(test "cGxlYXN1cmUu" (base64e "pleasure."))
(test "bGVhc3VyZS4=" (base64e "leasure."))
(test "ZWFzdXJlLg==" (base64e "easure."))

(test "cGxlYXN1cmUu" (base64-ros "pleasure."))
(test "bGVhc3VyZS4=" (base64-ros "leasure."))
(test "ZWFzdXJlLg==" (base64-ros "easure."))

(test (base64e "") "")
(test (base64e "f") "Zg==")
(test (base64e "fo") "Zm8=")
(test (base64e "foo") "Zm9v")
(test (base64e "foob") "Zm9vYg==")
(test (base64e "fooba") "Zm9vYmE=")
(test (base64e "foobar") "Zm9vYmFy")

(test (base64-ros "") "")
(test (base64-ros "f") "Zg==")
(test (base64-ros "fo") "Zm8=")
(test (base64-ros "foo") "Zm9v")
(test (base64-ros "foob") "Zm9vYg==")
(test (base64-ros "fooba") "Zm9vYmE=")
(test (base64-ros "foobar") "Zm9vYmFy")

#(bench
#  (do 500
#    (base64e test-str)
#    (wait 10)))

#(bench
#  (do 500
#    (base64-ros test-str)
#    (wait 10)))

(de rpadding (Stream)
  (let ((PSL PL) (tail 2 Stream))
    (cond
      ((and (= PSL "=") (= PL "=")) 4)
      ((= PL "=") 2)
      (T 0))))

(de rev-sextets (Stream)
  (mapcar 'get-bit Stream))

(de rev-octets (Stream)
  (make
    (while Stream
      (link (bin (pack (cut 8 'Stream)))))))

(de base64d (Str)
  (let (Stream (chop Str)
        Pad (rpadding Stream)
        Sexs (rev-sextets Stream)
        Bytes (chop (pack Sexs)))
    (con (tail (inc (/ Pad 2)) Bytes))
    (pipe
      (out NIL (apply 'wr (rev-octets Bytes)))
      (till NIL T))))

#(bench
#  (do 500
#    (base64d test-b64)
#    (wait 10)))
