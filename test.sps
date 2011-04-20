(import (rnrs)
        (mosh test)
        (mosh)
        (prefix (similarity cosine) cosine:)
        (prefix (tfidf) tfidf:))

(cosine:test)
(tfidf:test)

(test-results)
