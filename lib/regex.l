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

### Eflags

# If this bit is set, then the beginning-of-line operator doesn't match the beginning of the string (presumably because it's not the beginning of a line).
# If not set, then the beginning-of-line operator does match the beginning of the string.
(setq REG_NOTBOL 1)

# Like REG_NOTBOL, except for the end-of-line.
(setq REG_NOTEOL (>> -1 REG_NOTBOL))

### Error codes

# Success.
(setq REG_NOERROR 0)
# Didn't find a match (for regexec).
(setq REG_NOMATCH 1)

# POSIX regcomp return error codes.
# (In the order listed in the standard.)
(setq REG_BADPAT 2) # Invalid pattern.
(setq REG_ECOLLATE 3) # Not implemented.
(setq REG_ECTYPE 4) # Invalid character class name.
(setq REG_EESCAPE 5) # Trailing backslash.
(setq REG_ESUBREG 6) # Invalid back reference.
(setq REG_EBRACK 7) # Unmatched left bracket.
(setq REG_EPAREN 8) # Parenthesis imbalance.
(setq REG_EBRACE 9) # Unmatched \{.
(setq REG_BADBR 10) # Invalid contents of \{\}.
(setq REG_ERANGE 11) # Invalid range end.
(setq REG_ESPACE 12) # Ran out of memory.
(setq REG_BADRPT 13) # No preceding re for repetition op.

# Error codes we've added.
(setq REG_EEND 14) # Premature end.
(setq REG_ESIZE 15) # Compiled pattern bigger than 2^16 bytes.
(setq REG_ERPAREN 16) # Unmatched ) or \); not returned from regcomp.

### Native calls

# compile regex
(de regcomp (Pattern . @)
  (use Preg
    (let (Errcode (native "@" "regcomp" 'I '(Preg (64 B . 64)) Pattern (pass | 0)))
      (cons Preg Errcode))))

# match regex
(de regexec (Preg String Nmatch Pmatch Eflags)
  (native "@" "regexec" 'I (cons NIL (64) Preg) String Nmatch Pmatch Eflags))

# free mem
(de regfree (Preg)
  (native "@" "regfree" NIL (cons NIL (64) Preg)))

# error message
(de regerror (ErrCode Preg Size)
  (use Buf
    (let (Length (native "@" "regerror" 'I ErrCode (cons NIL (64) Preg) (cons 'Buf (64 C . 64) 0) Size))
      (cons Buf Length))))

### Tests

#(let (P "a" S "a" Preg (regcomp P))
#  (regexec Preg S 0 0 0)
#  (regfree Preg))

#(let (P "a" S "a" Preg (regcomp P))
#  (test REG_NOERROR (regexec Preg S 0 0 0)))
#(let (P "a" S "A" Preg (regcomp P))
#  (test REG_NOMATCH (regexec Preg S 0 0 0)))
#(let (P "a" S "A" Preg (regcomp P REG_ICASE))
#  (test REG_NOERROR (regexec Preg S 0 0 0)))
#(let (P "a[0-9]z" S "a7z" Preg (regcomp P))
#  (test REG_NOERROR (regexec Preg S 0 0 0)))
#(let (P "a[0-9z" S "a7z" Preg (regcomp P))
#  (test REG_BADPAT (regexec Preg S 0 0 0))
#  (regerror REG_BADPAT Preg 100))
#(let (P "a[0-9z" S "a7z" Preg (regcomp P))
#  (test REG_BADPAT (regexec Preg S 0 0 0)))
#(let (P "a\\" S "a" Preg (regcomp P))
#  (test REG_EESCAPE (regexec Preg S 0 0 0)))

### Wrappers

(de handle-error (Preg Errcode)
  (let (Size (cdr (regerror Errcode Preg 0))
        Msg (pack (car (regerror Errcode Preg Size))))
   (regfree Preg)
   (prinl "Regex error [" Errcode "]: " Msg)
   NIL))

(de re-compile (Pattern . @)
  (let (Res (pass 'regcomp Pattern)
        Preg (car Res)
        Errcode (cdr Res))
    (if (> Errcode 0)
      (handle-error Preg Errcode)
      Preg)))

(de re-match (Preg String)
  (let (Errcode (regexec Preg String 0 0 0))
    (if (> Errcode 0)
      (handle-error Preg Errcode)
      T)))

(de re-find (Pattern String . @)
  (let (Preg (pass 're-compile Pattern))
    (when Preg
      (re-match Preg String))))

### Tests

(test NIL (re-find "a[0-9z" "a7z"))
(test T (re-find "a[0-9]z" "a7z"))
(test NIL (re-find "a" "A"))
(test T (re-find "a" "A" REG_ICASE))
