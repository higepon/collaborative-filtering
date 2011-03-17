;; tfidf
;;
;;   tf : Term Frequency, idf : Inverse Document Frequency
(library (tfidf)
         (export tfidf test)
         (import (rnrs)
                 (shorten)
                 (mosh control)
                 (mosh test))

(define (tfidf . x)
  x)

(define (make-empty-stat)
  (make-hashtable string-hash string=?))

(define (stat-ref stat word)
  (hashtable-ref stat word #f))

(define (document-count stat word)
  (car (stat-ref stat word)))

(define (word-count stat word)
  (cdr (stat-ref stat word)))

(define (analyze document*)
  (let1 stat (make-empty-stat)
    (for-each
     (^d
      (analyze1 stat d))
     document*)
    stat))


(define (analyze1 stat document)
  #f)

(define (test)
  (let1 stat (make-empty-stat)
    (analyze1 stat "apple")
    (test-equal 1 (word-count stat "apple"))
    (test-equal 1 (document-count stat "apple")))
  (let1 stat (analyze '("apple orange apple" "apple"))
    (test-true stat)
    (test-equal 3 (word-count stat "apple"))
    (test-equal 2 (document-count stat "apple")))

  )





)
