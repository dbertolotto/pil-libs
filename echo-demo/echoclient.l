(setq PortN 12321)
(setq nprocs 3)
(setq ntimes 3)

(de child-handler ()
  (for i ntimes
    (let (Sock (connect "localhost" PortN))
      (when Sock
        (prinl (stamp) " | " *Pid " | Opened sock " Sock " on port " PortN)
        (out Sock
          (prinl "hello world"))
        (in Sock
          (until (eof)
            (out NIL
              (prinl (stamp) " | " *Pid " | " (line))))
          (close Sock)))))
  (bye))

(do nprocs
  (NIL (fork) (child-handler)))
(bye)
