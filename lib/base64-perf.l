(load+ "#lib/base64-test.l")

# wait is needed to avoid firing too many pipes at the same time

(bench
  (do 1000
    (base64e Test-txt)
    (wait 10)))

(bench
  (do 1000
    (base64d Test-b64)
    (wait 10)))
