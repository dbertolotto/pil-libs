(de add-auth-basic (Req User Pass)
  (let Head (get Req 'Headers)
    (push 'Head (cons "Authorization" (pack "Basic " User ': Pass)))
    Req))

(de mid-auth-basic (@Hand)
  (fill '((Req)
    (let (Res ('@Hand Req)
          Head (get Res 'Headers))
      (push 'Head "WWW-Authenticate" "Basic realm=\"User Visible Realm\", charset=\"UTF-8\"")
      Res))))

