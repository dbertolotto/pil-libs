(setq X509_FILETYPE_PEM      1
      X509_FILETYPE_ASN1     2
      X509_FILETYPE_DEFAULT  3)

(de ssl @
  (pass 'native "libssl.so"))

(de crypto @
  (pass 'native "libcrypto.so"))

(de init-openssl ()
  (ssl "SSL_load_error_strings" NIL NIL)
  (ssl "OpenSSL_add_all_algorithms" NIL NIL))

(de cleanup-openssl ()
  (ssl "EVP_cleanup" NIL NIL))

(de create-context ()
  (let (Method (ssl "SSLv23_server_method" 'N NIL)
        Context (ssl "SSL_CTX_new" 'N Method))
    (if (not Context)
      (prinl "Unable to create SSL context")
      Context)))

(de configure-context (Context)
  #(ssl "SSL_CTX_set_ecdh_auto" NIL Context 1)
  (let (Cert (ssl "SSL_CTX_use_certificate_file" 'I Context "/data/data/com.termux/files/home/pil/lib/cert/dummy.crt" X509_FILETYPE_PEM)
        Key (ssl "SSL_CTX_use_PrivateKey_file" 'I Context "/data/data/com.termux/files/home/pil/lib/cert/dummy.key" X509_FILETYPE_PEM))
    (if (<= Cert 0)
      (prinl "Unable to load certificate: " Cert))
    (if (<= Key 0)
      (prinl "Unable to load key: " Key))))

(de main ()
 (init-openssl)
 (let Context (create-context)
   (configure-context Context)
   (let (Port 4433
         Sock (accept (port 4433))
         Ssl (ssl "SSL_new" 'N Context))
     (ssl "SSL_set_fd" NIL Ssl Sock)
     (let Accept (ssl "SSL_accept" 'I Ssl)
       (if (<= Accept 0)
         (prinl "SSL error " Accept " " (ssl "SSL_get_error" Ssl Accept))
         (ssl "SSL_write" NIL Ssl "test\n" (length "test\n"))))
     (ssl "SSL_shutdown" NIL Ssl)
     (ssl "SSL_free" NIL Ssl)
     (close Sock))
   (ssl "SSL_CTX_free" NIL Context))
 (cleanup-openssl))

