(load "../../lib/string.l"
      "../../lib/util.l")
(symbols '(pico string util))

(de decode-char (C1 C2)
  (char (hex (pack C1 C2))))

(de decode-param (S)
  (pack
    (make
      (for (L (chop S) L)
        (let (C (pop 'L))
          (link
            (case C
              ("%" (decode-char (pop 'L) (pop 'L)))
              ("+" " ")
              (T C))))))))

(de parse-param (S)
  (let (Pair (splits S "="))
    (cons (intern (lowc (car Pair)))
          (decode-param (cadr Pair)))))

(de parse-params (S)
  (mapcar 'parse-param (splits S "&")))

(de parse-url (S)
  (let (Parts (filter nnil? (splits S " "))
        Url (splits (cadr Parts) "?"))
    (list (intern (uppc (car Parts)))
          (lowc (car Url))
          (parse-params (cadr Url))
          (caddr Parts))))

(de get-header ()
  (make
    (while (line T)
      (let (L (mapcar 'trims (splits @ ":")))
        (when L
          (link (cons (intern (car L)) (cadr L))))))))

(de get-content-params (Header)
  (let (Length (format (cdr (assoc 'content-length Header))))
    (when Length
      (parse-params (pack (head Length (till NIL)))))))

(de log @
  (pass 'prinl (stamp) " | " *Pid " | "))

(de server (Request-handler PortN)
  (let (Port (port PortN))
    (log "Server listening on port " PortN)
    (loop
      (setq Sock (listen Port))
      (NIL (fork) (child-handler Request-handler Port))
      (close Sock))))

(de child-handler (Request-handler Port)
   (close Port)
   (log "Client connected from " *Adr)
   (in Sock
     (until (eof)
       (let (Url-line (line T)
             Url (parse-url Url-line)
             Type (car Url)
             Path (cadr Url)
             Url-params (caddr Url)
             Version (cadddr Url)
             Header (get-header)
             Params (append Url-params (get-content-params Header))
             Response-body (Request-handler Type Path Header Params)
             Response (response Version 200 'OK '((content-type . "text/html; charset=utf-8")) Response-body))
         (prinl (stamp) " | " *Pid " | URL: " Url-line)
         (prinl (stamp) " | " *Pid " | Type: " Type)
         (prinl (stamp) " | " *Pid " | Path: " Path)
         (prinl (stamp) " | " *Pid " | URL-params: " Url-params)
         (prinl (stamp) " | " *Pid " | Version: " Version)
         (for P Header
           (prinl (stamp) " | " *Pid " | Header: " (car P) ": " (cdr P)))
         (for P Params
           (prinl (stamp) " | " *Pid " | Params: " (car P) ": " (cdr P)))
         (prinl Response-Body)
         (prinl Response)
         (out Sock
           (prinl Response)))
       (close Sock)))
   (prinl (stamp) " | " *Pid " | Client disconnected")
   (bye))

(de hello-request-handler (Type Path Header Params)
  (if (= Path "/greeting")
    (let (Name (assoc 'name Params))
      (if Name
        (text "<html>Nice to meet you, @1!</html>" (cdr Name))
        "<form>What is your name?<input name='name' /></form>"))
    "<html>Sorry... I don't know that page.</html>"))

(de response (Version Status Message Header Body)
  (pack Version " " Status " " Message "\n"
        (mapcar '((P) (pack (car P) ": " (cdr P) "\n")) Header)
        "\n"
        Body))

#(server hello-request-handler 8080)
#(bye)
