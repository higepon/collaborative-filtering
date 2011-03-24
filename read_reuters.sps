(import (rnrs)
        (mosh)
        (srfi :98)
        (mosh file)
        (tfidf))

(define corpus_dir (string-append (get-environment-variable "HOME") "/nltk_data/corpora/reuters/training"))

(display (directory-list corpus_dir))



