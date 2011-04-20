(import (rnrs)
        (mosh)
        (srfi :98)
        (only (srfi :1) take)
        (mosh file)
        (mosh control)
        (shorten)
        (tfidf))

(include "./text.dat")

(define
  (uniq lst)
  (let loop
       ((lst lst) (ret (quote ())))
       (cond ((null? lst) ret)
             (else (if (member (car lst) ret)
                       (loop (cdr lst) ret)
                       (loop (cdr lst) (cons (car lst) ret)))))))

(define corpus_dir (string-append (get-environment-variable "HOME") "/nltk_data/corpora/reuters/training"))

(define (corpus-name+path*)
  (map
   (^(name)
     (cons name (string-append corpus_dir "/" name)))
   (filter (^f (file-regular? (string-append corpus_dir "/" f))) (directory-list corpus_dir))))

(define (split doc)
  (string-split doc #\space))







#;(let loop ([name+path* (corpus-name+path*)]
           [stat '()])
  (cond
   [(null? stat)
    (loop (cdr name+path*) (analyze1 (caar name+path*) (split (file->string (cdar name+path*)))))]
   [(null? name+path*)
    (analyze1 stat 'earthquake-news1 (split earthquake-news2))
    (freeze-stat! stat)
    (serialize-stat stat "./hoge")
    (let1 word* (uniq (split earthquake-news2))
      (for-each (^w (write w) (newline)) (take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'earthquake-news1 w))) word*)) 10)))]
   [else
    (loop (cdr name+path*) (analyze1 stat (caar name+path*) (split (file->string (cdar name+path*)))))]))

(let1 stat (deserialize-stat "./hoge")
;  (analyze1 stat 'earthquake-news1 (split earthquake-news2))
  (let1 word* (uniq (split earthquake-news2))
(take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'earthquake-news1 w))) word*)) 10))))



;; (let1 document* (map (^f
;;                       (cons f (file->string (string-append corpus_dir "/" f))))
;;                      (filter (^f (file-regular? (string-append corpus_dir "/" f))) (take (directory-list corpus_dir) 7769)))
;;   (let1 stat (apply analyze (list document*))
;;     (analyze1 stat 'earthquake-news1 earthquake-news2)
;;     (let1 word* (uniq (string-split earthquake-news2 #\space))
;;       (for-each (^w (write w) (newline)) (take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'earthquake-news1 w))) word*)) 10))
;;     )))
