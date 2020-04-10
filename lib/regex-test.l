### Regex library tests
(load "unit.l" "regex.l")
(symbols '(pico unit regex))

### Tests
(detest t-compile-pass ()
  (check
    (not NIL (re-compile "a"))
    (not NIL (re-compile "[0-9]"))
    (not NIL (re-compile "\[0"))
    ))

(detest t-compile-fail ()
  (check
    (= NIL (re-compile "["))
    (= NIL (re-compile "{"))
    ))

(detest t-match-pass ()
  (check
    (= '("a") (re-match "a" "a"))
    (= '("A") (let *Icase T (re-match "a" "A")))
    (= '("6") (re-match "[0-9]" "678"))
    (= '("678") (re-match "[0-9]+" "678"))
    (= '("67") (re-match "[0-9]{2}" "678"))
    (= '("67") (re-match "[0-9]{1,2}" "678"))
    (= '("678") (re-match "[0-9]{1,4}" "678"))
    (= '(NIL) (re-match "[0-9]*" "a"))
    (= '("[") (re-match "\\[" "[]"))
    (= '("]") (re-match "\\]" "[]"))
    (= '("]") (re-match "]" "[]"))
    (= '("{") (re-match "\\{" "{}"))
    (= '("}") (re-match "\\}" "{}"))
    (= '("}") (re-match "}" "{}"))
    ))

(detest t-match-subs ()
  (check
    (= '("a" "a") (re-match "(a)" "baz"))
    (= '("ooba" "o" "a") (re-match "(o).*(a)" "foobar"))
    (= '("foobar" "oo" "a") (re-match "f([o]+).*(a)r" "foobar"))
    ))

(detest t-match-quick ()
  (let *Nmatch 0
    (check
      (= T (re-match "a" "a"))
      (= T (re-match ".*" "a"))
      )))

(detest t-match-fail ()
  (check
    (= NIL (re-match "a" "A"))
    (= NIL (re-match "[" "[]"))
    (= NIL (re-match "{" "{}"))
    ))

(detest t-compile ()
  (combine-results
    (t-compile-pass)
    (t-compile-fail)
    ))

(detest t-match ()
  (combine-results
    (t-match-pass)
    (t-match-subs)
    (t-match-quick)
    (t-match-fail)
    ))

(detest t-regex ()
  (combine-results
    (t-compile)
    (t-match)
    ))

(t-regex)

(bye)
