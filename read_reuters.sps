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

(define (hashtable-map proc ht)
  (let1 keys (vector->list (hashtable-keys ht))
    (map
     (lambda (key)
       (proc key (hashtable-ref ht key)))
     keys)))

(define (hashtable->alist ht)
  (hashtable-map cons ht))

(define (alist->string-hash-table alist)
  (let ([hashtable (make-hashtable string-hash string=?)])
    (for-each (lambda (x) (hashtable-set! hashtable (car x) (cdr x)))
              alist)
    hashtable))


(define (alist->eq-hash-table2 alist)
  (let ([hashtable (make-eq-hashtable)])
    (for-each (lambda (x) (hashtable-set! hashtable (car x) (alist->string-hash-table (cdr x))))
              alist)
    hashtable))


(define (serialize-stat stat name)
  (when (file-exists? name)
    (delete-file name))
;  (write (cons (map (^x (cons (car x) (hashtable->alist (cdr x)))) (hashtable->alist (car stat))) (hashtable->alist (cdr stat))) (open-output-file name)))
  (fasl-write (cons (map (^x (cons (car x) (hashtable->alist (cdr x)))) (hashtable->alist (car stat))) '()) (open-file-output-port name)))

(define (deserialize-stat name)
;  (let1 obj (read (open-input-file name))
  (let1 obj (fasl-read (open-file-input-port name))
;    (write obj)
    (cons (alist->eq-hash-table2 (car obj)) (alist->string-hash-table (cdr obj)))))

(let loop ([name+path* (corpus-name+path*)]
           [stat '()])
  (cond
   [(null? stat)
    (loop (cdr name+path*) (analyze1 (caar name+path*) (split (file->string (cdar name+path*)))))]
   [(null? name+path*)
    (serialize-stat stat "./hoge")
    (analyze1 stat 'earthquake-news1 (split earthquake-news2))
    (freeze-stat! stat)
    (let1 word* (uniq (split earthquake-news2))
      (for-each (^w (write w) (newline)) (take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'earthquake-news1 w))) word*)) 10)))]
   [else
    (loop (cdr name+path*) (analyze1 stat (caar name+path*) (split (file->string (cdar name+path*)))))]))

#;(let1 stat (deserialize-stat "./hoge")
  (analyze1 stat 'earthquake-news1 (split earthquake-news2))
  (let1 word* (uniq (split earthquake-news2))
    (for-each (^w (write w) (newline)) (take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'earthquake-news1 w))) word*)) 10))))



;; (let1 document* (map (^f
;;                       (cons f (file->string (string-append corpus_dir "/" f))))
;;                      (filter (^f (file-regular? (string-append corpus_dir "/" f))) (take (directory-list corpus_dir) 7769)))
;;   (let1 stat (apply analyze (list document*))
;;     (analyze1 stat 'earthquake-news1 earthquake-news2)
;;     (let1 word* (uniq (string-split earthquake-news2 #\space))
;;       (for-each (^w (write w) (newline)) (take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'earthquake-news1 w))) word*)) 10))
;;     )))
