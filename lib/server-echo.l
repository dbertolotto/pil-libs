(load+ "#lib/server.l")
(symbols 'pico 'server)

(de echo-handler (ReaderOut)
  (prinl "Server echo: " ReaderOut))

(de echo-reader ()
  (line T))

(de echo-server (Port)
  (generic-server Port 'echo-reader 'echo-handler))

(de -server ()
  (echo-server 8080))

(de echo-client (Host Port)
  (prinl "Echo Client - press :q to quit")
  (while T
  (let? Sock (connect Host Port)
    (out NIL
      (prin "Client says: "))
    (let Input (in NIL (read " :"))
      (when (= Input ":q") (bye))
      (out Sock (prinl Input)))
    (in Sock (until (eof) (prinl (line T)))))))

(de -client ()
  (echo-client 'localhost 8080))
