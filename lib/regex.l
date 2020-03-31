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

### Native calls

# compile regex
(de regcomp (Pattern . @)
  (use Preg
    (native "@" "regcomp" 'I '(Preg (64 B . 64)) Pattern (pass | 0))
    Preg))

# match regex
(de regexec (Preg String Nmatch Pmatch Eflags)
  (native "@" "regexec" 'I (cons NIL (64) Preg) String Nmatch Pmatch Eflags))

(let (Pat "a[0-9]z" String "a7z")
  (let (Preg (regcomp Pat REG_EXTENDED))
    (when (=0 (regexec Preg String 0 0 0))
      (prinl "String \"" String "\" matches regex \"" Pat "\""))))
