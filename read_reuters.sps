(import (rnrs)
        (mosh)
        (srfi :98)
        (only (srfi :1) take)
        (mosh file)
        (mosh control)
        (shorten)
        (tfidf))

(include "./text.dat")

(define corpus_dir (string-append (get-environment-variable "HOME") "/nltk_data/corpora/reuters/training"))

(let1 document* (map (^f
                      (cons f (file->string (string-append corpus_dir "/" f))))
                     (filter (^f (file-regular? (string-append corpus_dir "/" f))) (take (directory-list corpus_dir) 100)))
  (let1 stat (apply analyze (list document*))
    (analyze1 stat 'earthquake-news1 earthquake-news1)
    (format #t "catastrophe(tfidf)=~a and(tfidf)=~a" (tf-idf stat 'earthquake-news1 "Amano")
            (tf-idf stat 'earthquake-news1 "and"))
    ))
