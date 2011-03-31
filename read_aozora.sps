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

(let* ([m (mecab-new2 "")]
       [text (string->utf8 (file->string (cdar (corpus-name+path*))))]
       [len (bytevector-length text)])
  (write (mecab-node-surface* (mecab-sparse-tonode2 m text len))))