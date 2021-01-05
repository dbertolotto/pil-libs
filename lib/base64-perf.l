(load "base64-test.l")

# wait is needed to avoid firing too many pipes at the same time

(bench
  (do 1000
    (base64~encode Test-txt)
    (wait 1)))

(bench
  (do 1000
    (base64~decode Test-b64)
    (wait 1)))
