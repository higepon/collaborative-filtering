(import (rnrs)
        (mosh test)
        (mosh)
        (prefix (similarity cosine) cosine:)
        (prefix (tfidf) tfidf:))

;; todo split n-gram
(include "./text.dat")

(cosine:test)
(tfidf:test)

(test-equal 1.0 (cosine:similarity "apple" "apple"))
(test-true (> (cosine:similarity earthquake-news1 earthquake-news2)
              (cosine:similarity earthquake-news1 it-news)))
(write (cosine:similarity earthquake-news1 earthquake-news2))
(newline)
(write               (cosine:similarity earthquake-news1 it-news))
(test-results)
