(load "base64.l")

(setq Test-txt "Man is distinguished, not only by his reason, but by this singular passion from other animals,
which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable
generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

(setq Test-b64 "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLAp3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlCmdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")

# encoding

(test Test-b64 (base64~encode Test-txt))
(test "4a2W" (base64~encode (char 6998)))

(test "cGxlYXN1cmUu" (base64~encode "pleasure."))
(test "bGVhc3VyZS4=" (base64~encode "leasure."))
(test "ZWFzdXJlLg==" (base64~encode "easure."))

(test (base64~encode "")       "")
(test (base64~encode "f")      "Zg==")
(test (base64~encode "fo")     "Zm8=")
(test (base64~encode "foo")    "Zm9v")
(test (base64~encode "foob")   "Zm9vYg==")
(test (base64~encode "fooba")  "Zm9vYmE=")
(test (base64~encode "foobar") "Zm9vYmFy")

# decoding

(test Test-txt (base64~decode Test-b64))
(test (char 6998) (base64~decode "4a2W"))

(test (base64~decode "cGxlYXN1cmUu") "pleasure.")
(test (base64~decode "bGVhc3VyZS4=") "leasure.")
(test (base64~decode "ZWFzdXJlLg==") "easure.")

#(test ""       (base64~decode ""))
(test "f"      (base64~decode "Zg=="))
(test "fo"     (base64~decode "Zm8="))
(test "foo"    (base64~decode "Zm9v"))
(test "foob"   (base64~decode "Zm9vYg=="))
(test "fooba"  (base64~decode "Zm9vYmE="))
(test "foobar" (base64~decode "Zm9vYmFy"))

