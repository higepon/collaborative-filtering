(import (rnrs)
        (mosh test)
        (mosh)
        (similarity cosine))

;; todo split n-gram
(include "./text.dat")

(test)

(test-equal 1.0 (similarity "apple" "apple"))

(test-true (> (similarity earthquake-news1 earthquake-news2)
              (similarity earthquake-news1 it-news)))

(write (similarity earthquake-news1 earthquake-news2))
(newline)
(write               (similarity earthquake-news1 it-news))
(test-results)
