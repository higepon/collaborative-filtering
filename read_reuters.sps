(import (rnrs)
        (mosh)
        (srfi :98)
        (mosh file)
        (shorten)
        (tfidf))

(define corpus_dir (string-append (get-environment-variable "HOME") "/nltk_data/corpora/reuters/training"))

(apply analyze (list (map
          (^f
           (cons f (file->string (string-append corpus_dir "/" f))))
          (filter (^f (file-regular? (string-append corpus_dir "/" f))) (directory-list corpus_dir)))))
