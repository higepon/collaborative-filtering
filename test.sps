(import (rnrs)
        (mosh test)
        (mosh)
        (similarity cosine))

;; todo split n-gram
(include "./text.dat")

(test)

(test-true (> (similarity earthquake-news1 earthquake-news2)
              (similarity earthquake-news1 it-news)))


(write (similarity earthquake-news1 earthquake-news2))
(write               (similarity earthquake-news1 it-news))
(test-results)
