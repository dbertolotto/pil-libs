(symbols 'intro 'pico)

# return a list of functions in namespace
(local nsp-fn)
(de nsp-fn (Nsp)
  (fish getd Nsp))
# e.g. (nsp-fn pico)
