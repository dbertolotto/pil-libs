(setq *LibSSL "/data/data/com.termux/files/usr/lib/libssl.so")

(setq *LibCrypto "/data/data/com.termux/files/usr/lib/libcrypto.so")

(setq X509-filetype-pem 1)

(de openssl @
  (pass 'native *LibSSL))

(de crypto @
  (pass 'native *LibCrypto))

(de add-all-alg ()
  (crypto "OpenSSL_add_all_algorithms" NIL))

(de ssl-load-err-str ()
  (openssl "SSL_load_error_strings" NIL))

(de crypto-load-err-str ()
  (crypto "ERR_load_crypto_strings" NIL))

(de err-free-str ()
  (crypto "ERR_free_strings" NIL))

(de tls-v12-server ()
  (openssl "TLSv1_2_server_method" 'N))

(de tls-v12-client ()
  (openssl "TLSv1_2_client_method" 'N))

(de ctx-new (M)
  (openssl "SSL_CTX_new" 'N M))

(de ssl-ctx-use-cert-file (Ctx File Type)
  (openssl "SSL_CTX_use_certificate_file" 'I Ctx File Type)) 

(de ssl-ctx-use-pk-file (Ctx File Type)
  (openssl "SSL_CTX_use_PrivateKey_file" 'I Ctx File Type)) 

(de ssl-check-pk (Ssl)
  (openssl "SSL_check_private_key" 'I Ssl))

(de ssl-ctx-check-pk (Ctx)
  (openssl "SSL_CTX_check_private_key" 'I Ctx))

(de ssl-new (Ctx)
  (openssl "SSL_new" 'N Ctx))

(de ssl-set-fd (Ssl FD)
  (openssl "SSL_set_fd" 'I Ssl FD))

(de ssl-accept (Ssl)
  (openssl "SSL_accept" 'I Ssl))

(de ssl-read (Ssl Buf Size)
  (openssl "SSL_read" 'I Ssl Buf Size))

(de ssl-write (Ssl Buf Size)
  (openssl "SSL_write" 'I Ssl Buf Size))

(de ssl-free (Ssl)
  (openssl "SSL_free" NIL Ssl))

(setq M (tls-v12-server))
(setq C (ctx-new M))
(ssl-ctx-use-cert-file C "cert/server.key" X509-filetype-pem) 
(ssl-ctx-use-pk-file C "cert/server.crt" X509-filetype-pem)
(ssl-ctx-check-pk C)

(setq PortN 12321)
(setq Port (port PortN))
(setq Sock (listen Port))

(setq S (ssl-new C))
(ssl-set-fd S Sock)
(ssl-accept S)

