(import (rnrs)
        (mosh)
        (srfi :98)
        (only (srfi :1) take)
        (mosh file)
        (mosh control)
        (shorten)
        (mecab)
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

(define corpus_dir (string-append (get-environment-variable "HOME") "/nltk_data/corpora/aozora"))

(define (corpus-name+path*)
  (map
   (^(name)
     (cons name (string-append corpus_dir "/" name)))
   (filter (^f (file-regular? (string-append corpus_dir "/" f))) (directory-list corpus_dir))))

(let loop ([name+path* (corpus-name+path*)]
           [stat '()])
  (cond
   [(null? stat)
    (loop (cdr name+path*) (analyze1 (caar name+path*) (file->string (cdar name+path*))))]
   [(null? name+path*)
    (fasl-write stat (open-file-output-port "./hoge"))
    (analyze1 stat 'earthquake-news1 earthquake-news2)
    (let1 word* (uniq (string-split earthquake-news2 #\space))
      (for-each (^w (write w) (newline)) (take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'earthquake-news1 w))) word*)) 10)))]
   [else
    (loop (cdr name+path*) (analyze1 stat (caar name+path*) (file->string (cdar name+path*))))]))

;; (let* ([m (mecab-new2 "")]
;;        [text (string->utf8 (file->string (cdar (corpus-name+path*))))]
;;        [len (bytevector-length text)])
;;   (write (mecab-node-surface* (mecab-sparse-tonode2 m text len))))
