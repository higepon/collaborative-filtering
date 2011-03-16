(import (rnrs)
        (mosh test)
        (mosh)
        (prefix (similarity cosine) cosine:))

;; todo split n-gram
(include "./text.dat")

(cosine:test)

(test-equal 1.0 (cosine:similarity "apple" "apple"))
(test-true (> (cosine:similarity earthquake-news1 earthquake-news2)
              (cosine:similarity earthquake-news1 it-news)))
(write (cosine:similarity earthquake-news1 earthquake-news2))
(newline)
(write               (cosine:similarity earthquake-news1 it-news))
(test-results)
