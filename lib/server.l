(symbols 'server 'pico)

(local) *Timeout
(setq
  *Timeout 5000) # ms

(local) server-task
(de server-task (Port @reader @handler)
  (macro
    (task (port Port)
      (when (accept @)
        (task @
          Sock @
          (out Sock
            (@handler
              (in Sock
                (@reader))))
          (task Sock) # close task
          (close Sock)))))) # close sock

(local) server-fork
(de server-fork (Port reader handler)
  (let (Pfd (port Port)
        Sock NIL
        CPid NIL)
    (loop
      (setq Sock (listen Pfd))
      (setq CPid (fork))
      (NIL CPid (server-child-handler Pfd Sock reader handler))
      (install-timeout CPid)
      (close Sock))))

(de install-timeout (CPid)
  (when CPid
    (let TaskNum (- *Timeout CPid)
      (task TaskNum *Timeout
        P CPid
        T *Timeout
        N TaskNum
        (when (member P (kids))
          (kill P)
          (prinl (text "Child process @1 timed out after @2 s" P (/ T 1000))))
        (task N)))))

(local) server-child-handler
(de server-child-handler (Pfd Sock reader handler)
  (prinl (text "Forked pid @1 from parent @2" *Pid *PPid))
  (close Pfd)
  (out Sock
    (handler
      (in Sock
        (reader))))
  (close Sock)
  (bye))

(local) generic-server
#(def 'generic-server server-task)
(def 'generic-server server-fork)

