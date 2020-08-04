(load+ "#lib/base64.l")

(setq Test-txt "Man is distinguished, not only by his reason, but by this singular passion from other animals,
which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable
generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

(setq Test-b64 "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLAp3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlCmdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")

# encoding

(test Test-b64 (base64e Test-txt))
(test "4a2W" (base64e (char 6998)))

(test "cGxlYXN1cmUu" (base64e "pleasure."))
(test "bGVhc3VyZS4=" (base64e "leasure."))
(test "ZWFzdXJlLg==" (base64e "easure."))

(test (base64e "")       "")
(test (base64e "f")      "Zg==")
(test (base64e "fo")     "Zm8=")
(test (base64e "foo")    "Zm9v")
(test (base64e "foob")   "Zm9vYg==")
(test (base64e "fooba")  "Zm9vYmE=")
(test (base64e "foobar") "Zm9vYmFy")

# decoding

(test Test-txt (base64d Test-b64))
(test (char 6998) (base64d "4a2W"))

(test (base64d "cGxlYXN1cmUu") "pleasure.")
(test (base64d "bGVhc3VyZS4=") "leasure.")
(test (base64d "ZWFzdXJlLg==") "easure.")

#(test ""       (base64d ""))
(test "f"      (base64d "Zg=="))
(test "fo"     (base64d "Zm8="))
(test "foo"    (base64d "Zm9v"))
(test "foob"   (base64d "Zm9vYg=="))
(test "fooba"  (base64d "Zm9vYmE="))
(test "foobar" (base64d "Zm9vYmFy"))

