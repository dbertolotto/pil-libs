(setq PortN 12321)
(setq Port (port PortN))
(prinl "Echo server (Pid " *Pid ") listening on port " PortN) 

(de child-handler ()
  (close Port)
  (prinl (stamp) " | " *Pid " | Client connected from " *Adr)
  (in Sock
    (until (eof)
      (out Sock 
        (prinl "echo: " (line)))
      (close Sock)))
  (prinl (stamp) " | " *Pid " | Client disconnected")
  (bye))

(loop
  (setq Sock (listen Port)) # Listen
  (NIL (fork) (child-handler)) # Accepted
  (close Sock)) # Parent: Close socket and continue
