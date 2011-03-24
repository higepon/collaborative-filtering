;; tfidf
;;
;;   tf : Term Frequency, idf : Inverse Document Frequency
(library (tfidf)
         (export test analyze analyze1 tf-idf)
         (import (rnrs)
                 (shorten)
                 (match)
                 (mosh)
                 (mosh control)
                 (mosh test))

(define-record-type stat
  (fields (immutable doc)
          (immutable all))
  (protocol
   (lambda (c)
     (lambda ()
       (c (make-eq-hashtable) (make-string-hashtable))))))

(define (stat-all-inc! stat word)
  (let1 all (stat-all stat)
    (hashtable-set! all word (+ 1 (hashtable-ref all word 0)))))

(define (stat-doc-inc! stat doc-id word)
  (cond
   [(hashtable-ref (stat-doc stat) doc-id #f) =>
    (lambda (doc)
      (hashtable-set! doc word (+ 1 (hashtable-ref doc word 0))))]
   [else
    (let1 doc (make-string-hashtable)
      (hashtable-set! (stat-doc stat) doc-id doc)
      (hashtable-set! doc word 1))]))

(define (tf-idf stat doc-id word)
  (* (tf stat doc-id word) (idf stat word)))

(define (tf stat doc-id word)
  (let1 total (hashtable-fold-left (^(seed w count)
                                   (+ seed count))
                                 0 (hashtable-ref (stat-doc stat) doc-id #f))
    (/ (word-count stat doc-id word) total)))

(define (idf stat word)
  (let ([num-documens (hashtable-size (stat-doc stat))]
        [num-doc-include-word (hashtable-fold-left (^(seed doc-id st)
                                                     (+ seed (if (hashtable-ref st word #f) 1 0)))
                                         0 (stat-doc stat))])
    (log (/ num-documens (+ 1 num-doc-include-word)))))

(define (make-string-hashtable)
  (make-hashtable string-hash string=?))

(define (document-count stat word)
  (hashtable-fold-left
   (^(seed key st)
     (+ seed (if (hashtable-ref st word #f) 1 0)))
   0 (stat-doc stat)))

(define (word-count stat doc-id word)
  (cond
   [(hashtable-ref (stat-doc stat) doc-id #f) =>
    (lambda (x)
      (hashtable-ref x word 0))]
   [else
    (error 'word-count "unknown doc-id" doc-id)]))

(define (analyze document*)
  (let1 stat (make-stat)
    (for-each
     (match-lambda
      [(doc-id . document)
       (analyze1 stat doc-id document)])
     document*)
    stat))

(define analyze1
  (match-lambda*
   [(stat doc-id document)
    (let ([word* (string-split document #\space)])
      (for-each
       (^w
        (stat-all-inc! stat w)
        (stat-doc-inc! stat doc-id w))
       word*)
      stat)]
   [(doc-id document)
    (analyze1 (make-stat) document document)]))

(define (test)
  (let1 stat (make-stat)
    (analyze1 stat 'doc1 "apple")
    (test-equal 1 (word-count stat 'doc1 "apple"))
    (test-equal 1 (document-count stat "apple")))
  (let1 stat (analyze '((doc1 . "apple orange apple")
                        (doc2 . "apple")))
    (test-true stat)
    (test-equal 2 (word-count stat 'doc1 "apple"))
    (test-equal 1 (word-count stat 'doc2 "apple"))
    (test-equal 2 (document-count stat "apple"))
    (test-equal 2/3 (tf stat 'doc1 "apple"))
    (test-true (good-enough? (log (/ 2 3)) (idf stat "apple")))
    (test-true (good-enough? (log (/ 2 2)) (idf stat "orange")))
    (test-true (> (tf-idf stat 'doc1 "orange") (tf-idf stat 'doc1 "apple")))
  ))


)
