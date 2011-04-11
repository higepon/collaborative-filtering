;; tfidf
;;
;;   tf : Term Frequency, idf : Inverse Document Frequency
(library (tfidf)
         (export test analyze analyze1 tf-idf tf freeze-stat!)
         (import (rnrs)
                 (shorten)
                 (match)
                 (mosh)
                 (mosh control)
                 (mosh test))

(define (stat-doc stat)
  (car stat))

(define (stat-count stat)
  (cdr stat))

(define (make-stat)
  (cons (make-eq-hashtable) (make-string-hashtable)))

(define (stat-doc-inc! stat doc-id word)
  (cond
   [(hashtable-ref (stat-doc stat) doc-id #f) =>
    (lambda (doc)
      (hashtable-set! doc word (+ 1 (hashtable-ref doc word 0))))]
   [else
    (let1 doc (make-string-hashtable)
      (hashtable-set! (stat-doc stat) doc-id doc)
      (hashtable-set! doc word 1))]))

(define (stat-add-for-count! stat doc-id word)
  (hashtable-set! (stat-count stat) word (cons doc-id (hashtable-ref (stat-count stat) word '()))))

(define (tf-idf stat doc-id word)
  (* (tf stat doc-id word) (idf stat word)))

(define (tf stat doc-id word)
  (let1 total (hashtable-fold-left (^(seed w count)
                                   (+ seed count))
                                 0 (hashtable-ref (stat-doc stat) doc-id #f))
    (/ (word-count stat doc-id word) total)))

(define (idf stat word)
  (let ([num-documens (hashtable-size (stat-doc stat))]
        [num-doc-include-word (document-count stat word)])
    (log (/ num-documens (+ 1 num-doc-include-word)))))

(define (make-string-hashtable)
  (make-hashtable string-hash string=?))

;; move
(define
  (uniq lst)
  (let loop
       ((lst lst) (ret (quote ())))
       (cond ((null? lst) ret)
             (else (if (member (car lst) ret)
                       (loop (cdr lst) ret)
                       (loop (cdr lst) (cons (car lst) ret)))))))


(define (document-count stat word)
  (let1 x (hashtable-ref (stat-count stat) word '())
    (if (pair? x)
        (length (uniq x))
        x)))
  ;; (hashtable-fold-left
  ;;  (^(seed key st)
  ;;    (+ seed (if (hashtable-ref st word #f) 1 0)))
  ;;  0 (stat-doc stat)))

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
      [(doc-id . word*)
       (analyze1 stat doc-id word*)])
     document*)
    stat))

(define (freeze-stat! stat)
  (let ([ht (stat-count stat)])
    (hashtable-for-each
     (^(key value)
       (hashtable-set! ht key (length (uniq value))))
     ht)))

(define analyze1
  (match-lambda*
   [(stat doc-id word*)
    (for-each
     (^w
      (stat-doc-inc! stat doc-id w)
      (stat-add-for-count! stat doc-id w))
     word*)
      stat]
   [(doc-id word*)
    (analyze1 (make-stat) doc-id word*)]))

(define (test)
  (let1 stat (make-stat)
    (analyze1 stat 'doc1 '("apple"))
    (test-equal 1 (word-count stat 'doc1 "apple"))
    (test-equal 1 (document-count stat "apple")))
  (let1 stat (analyze '((doc1 . ("apple" "orange" "apple"))
                        (doc2 . ("apple"))))
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
