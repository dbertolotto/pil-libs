### Regex library
(local) '(re-compile re-execute re-match)

### Globals

# max number of subexpression matches
(setq *Nmatch 10)

# cflags settings T/NIL
(setq
  *Extended T
  *Icase NIL
  *Newline NIL
  *Nosub NIL)

# eflags settings T/NIL
(setq
  *Bol T
  *Eol T)

### Cflags

# If this bit is set, then use extended regular expression syntax.
# If not set, then use basic regular expression syntax.
(setq REG_EXTENDED 1)

# If this bit is set, then ignore case when matching.
# If not set, then case is significant.
(setq REG_ICASE (>> -1 REG_EXTENDED))

# If this bit is set, then anchors do not match at newline characters in the string.
# If not set, then anchors do match at newlines.
(setq REG_NEWLINE (>> -1 REG_ICASE))

# If this bit is set, then report only success or fail in regexec.
# If not set, then returns differ between not matching and errors.
(setq REG_NOSUB (>> -1 REG_NEWLINE))

(de build-cflags ()
  (| (if *Extended REG_EXTENDED 0)
     (if *Icase REG_ICASE 0)
     (if *Newline REG_NEWLINE 0)
     (if Nosub REG_NOSUB 0)))

### Eflags

# If this bit is set, then the beginning-of-line operator doesn't match the beginning of the string (presumably because it's not the beginning of a line).
# If not set, then the beginning-of-line operator does match the beginning of the string.
(setq REG_NOTBOL 1)

# Like REG_NOTBOL, except for the end-of-line.
(setq REG_NOTEOL (>> -1 REG_NOTBOL))

(de build-eflags ()
  (| (if *Bol 0 REG_NOTBOL)
     (if *Eol 0 REG_NOTEOL)))

### Native calls

# compile regex
(de regcomp (Pattern)
  (use Preg
    (let (Errcode (native "@" "regcomp" 'I '(Preg (64 B . 64) 0) Pattern (build-cflags)))
      (cons Preg Errcode))))

# match regex
(de build-pmatch (Nmatch)
  (when (> Nmatch 0)
    (list 'Pmatch (cons Nmatch '(I . 2) Nmatch) 0)))

(de regexec (Preg String)
  (use Pmatch
    (let (Errcode (native "@" "regexec" 'I (cons NIL (64) Preg) String *Nmatch (build-pmatch *Nmatch) (build-eflags)))
      (cons Pmatch Errcode))))

# free mem
(de regfree (Preg)
  (native "@" "regfree" NIL (cons NIL (64) Preg)))

# error message
(de regerror (ErrCode Preg Size)
  (use Buf
    (let (Length (native "@" "regerror" 'I ErrCode (cons NIL (64) Preg) (cons 'Buf (64 C . 64) 0) Size))
      (cons Buf Length))))

### Helpers

(de handle-error (Preg Errcode Ident)
  (when (> Errcode 0)
    (let (Size (cdr (regerror Errcode Preg 0))
          Msg (pack (car (regerror Errcode Preg Size))))
      (regfree Preg)
      (prinl "Regex " Ident " error [" Errcode "]: " Msg)
      NIL)))

(de extract-matches (String Pmatch)
  (mapcar '((M) (let (S (chop String)
                      B (car M)
                      E (cadr M))
                  (pack (tail (- E B) (head E S)))))
          (filter '((M) (not (= (car M) -1))) Pmatch)))

### Wrappers

(de re-compile (Pattern)
  (let ((Preg . Errcode) (regcomp Pattern))
    (case Errcode
      (0 Preg)
      (T (handle-error Preg Errcode "compile")))))

(de re-execute (Preg String)
  (let ((Pmatch . Errcode) (regexec Preg String))
    (case Errcode
      (0 (if (extract-matches String Pmatch) @ T))
      (1 NIL)
      (T (handle-error Preg Errcode "match")))))

(de re-match (Pattern String)
  (when (re-compile Pattern)
    (re-execute @ String)))

