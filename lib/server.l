#(symbols 'server 'pico)

(setq
  *Timeout 30000)

#(local) server
(de server (Port @Reader @Handler)
  (macro
    (task (port Port)
      (when (accept @)
        (task @
          Sock @
          (in Sock
            (out Sock
              (@Handler (@Reader)))
              (task Sock) # close task
              (close Sock) # close sock
              ))))))

(de server2 (Port Reader Handler)
  (let (Pfd (port Port)
        Sock NIL)
    (loop
      (setq Sock (listen Pfd))
      (NIL (fork) (child-handler Pfd Sock Reader Handler))
      (close Sock))))

(de child-handler (Pfd Sock Reader Handler)
  (close Pfd)
  (timeout *Timeout)
  (in Sock
    (out Sock
      (Handler (Reader)))
    (close Sock)))

(de echo-handler (ReaderOut)
  (prinl "Server echo: " ReaderOut))

(de echo-reader ()
  (line T))

(de client (Host Port)
  (prinl "Echo Client - press :q to quit")
  (while T
  (let? Sock (connect Host Port)
    (out NIL
      (prin "Client says: "))
    (let Input (in NIL (read " :"))
      (when (= Input ":q") (bye))
      (out Sock (prinl Input)))
    (in Sock (until (eof) (prinl (line T)))))))

(de echo-server (Port)
  (server2 Port 'echo-reader 'echo-handler))
