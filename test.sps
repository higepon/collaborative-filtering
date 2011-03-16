(import (rnrs)
        (mosh test)
        (mosh)
        (similarity cosine))

;; todo split n-gram
(include "./text.dat")

(test-true (> (similarity earthquake-news1 earthquake-news2)
              (similarity earthquake-news1 it-news)))

(test-results)
