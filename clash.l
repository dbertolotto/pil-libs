(load "json.l" "@lib/http.l" "@lib/xhtml.l")

(setq *Token (in "token.txt" (line T)))
(setq
  *Clan "#G2RYVP"
  *Player "#RUGCQJ9")

(setq *BaseUrl "https://api.clashroyale.com")
(setq *ApiVersion "v1")

# see https://www.w3schools.com/tags/ref_urlencode.asp
(setq *Encodings '((" " 20) ("#" 23)))

(de encode-char (C)
  (let (E (assoc C *Encodings))
    (if E (pack "%" (cadr E)) C)))

(de encode-url (S)
  (pack (mapcar 'encode-char (chop S))))

(de api-url @
  (glue "/" (pass 'list *BaseUrl *ApiVersion)))

(de api-opt (K V)
  (pack K "=" V))

(de api-opts (L)
  (let (K (mix L ~(range 1 (length L) 2))
        V (mix L ~(range 2 (length L) 2)))
    (pack "?" (glue "&" (mapcar 'api-opt K V)))))

(de api-get (Url . @)
  (let (Cmd (list 'curl "-q" "-s" "-X" "GET" "--header" "Accept: application/json" "--header" (pack "authorization: Bearer " *Token) (pass 'pack Url)))
    (in Cmd (till NIL T))))

(de get-cards ()
  (api-get (api-url "cards")))

(de get-player (Tag)
  (api-get (api-url "players" (encode-url Tag))))

(de get-clan (Tag) 
  (api-get (api-url "clans" (encode-url Tag))))

(de get-clan-members (Tag . Opts)
  (api-get (api-url "clans" (encode-url Tag) "members") (api-opts Opts)))

(de get-clan-warlog (Tag . Opts)
  (api-get (api-url "clans" (encode-url Tag) "warlog") (api-opts Opts)))

(de get-clan-currentwar (Tag)
  (api-get (api-url "clans" (encode-url Tag) "currentwar")))

(de cards ()
  (readJsonStr (get-cards)))

(de player ()
  (readJsonStr (get-player *Player)))

(de clan ()
  (readJsonStr (get-clan *Clan)))

(de curr-war ()
  (readJsonStr (get-clan-currentwar *Clan)))

(de warlog (n)
  (readJsonStr (get-clan-warlog *Clan 'limit n)))

(de members ()
  (readJsonStr (get-clan-members *Clan)))

(de $datetime (S)
  (let (L (mapcar 'pack (split (car (split (chop S) ".")) "T")))
    (list ($dat (car L)) ($tim (cadr L)))))

(de datetime (S) 
  (let (dt ($datetime S))
    (stamp (car dt) (cadr dt))))

(de participants (War)
  (jget War 'participants))

(setq participants-h '("tag" "name" "cards" "BP" "W" "CDBP" "NB"))

(setq member-h '("tag" "name" "role" "lastSeen" "level" "trophies" "arena" "rank" "prevRank" "don" "donRec"))

(de member-v (M)
  (list
    (jget M 'tag)
    (jget M 'name)
    (jget M 'role)
    (datetime (jget M 'lastSeen))
    (jget M 'expLevel)
    (jget M 'trophies)
    (jget M 'arena 'name)
    (jget M 'clanRank)
    (jget M 'previousClanRank)
    (jget M 'donations)
    (jget M 'donationsReceived)))

(de table-head-plain (Head)
  (mapcar '((X) (cons NIL X)) Head))

(de members-table (Members)
  (let (M (jget Members 'items)
        head member-h
        rows (mapcar 'member-v M))
    (html 0 "Current War" "@lib.css" NIL
      (<h1> NIL "Current War")
      (<br>)
      (<table> NIL NIL (table-head-plain head)       
        (for r rows                     
          (apply '<row> r NIL))))))

(de participants-table (War)
  (let (state (jget War 'state)
        dt (datetime (jget War 'collectionEndTime))
        P (jget War 'participants)
        head participants-h
        rows (mapcar 'jvals P))
    (html 0 "Current War" "@lib.css" NIL
      (<h1> NIL "Current War")
      (<h2> NIL state " - " dt)
      (<br>)
      (<table> NIL NIL (table-head-plain head)
        (for r rows
            (apply '<row> r NIL))))))

(de war-tables (Warlog)
  (html 0 "Wars" "@lib.css" NIL
    (for W (jget Warlog 'items)
      (let (S (jget W 'seasonId)
            dt (datetime (jget W 'createdDate))
            P (jget W 'participants)
            head participants-h
            rows (mapcar 'jvals P))
        (<h1> NIL "Season " S " - " dt)
        (<br>)
        (<table> NIL NIL (table-head-plain head)
          (for r rows
            (apply '<row> r NIL)))))))

(de error-page ()
  (html 0 "Error" "@lib.css" NIL
    (<h1> NIL "404 Not Found")))

(redef http (S)
  (let (Line (mapcar 'pack (split (chop (in S (line))) " "))
        Typ (car Line)
        Url (cadr Line))
    (if (= Typ "GET")
      (out S (cond
        ((= Url "/currwar") (out S (participants-table (curr-war))))
        ((= Url "/wars") (out S (war-tables (warlog 5))))
        ((= Url "/members") (out S (members-table (members))))
        (T (error-page))))
      (out S (error-page)))))

(server 8081)
