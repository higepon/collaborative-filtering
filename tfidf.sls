;; tfidf
;;
;;   tf : Term Frequency, idf : Inverse Document Frequency
(library (tfidf)
         (export tfidf test)
         (import (rnrs)
                 (shorten)
                 (mosh)
                 (mosh control)
                 (mosh test))

(define (tfidf . x)
  x)

(define (make-empty-stat)
  (make-hashtable string-hash string=?))

(define (stat-ref stat word)
  (hashtable-ref stat word #f))

(define (stat-add! stat word s)
  (hashtable-set! stat word s))

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
  (let ([word* (string-split document #\space)]
        [seen (make-empty-stat)])
    (for-each
     (^w
      (cond
       [(stat-ref stat w) =>
        (lambda (x)
          (cond
           [(stat-ref seen w)
            (stat-add! stat w (cons (car x) (+ 1 (cdr x))))]
           [else
            (stat-add! seen w #t)
            (stat-add! stat w (cons (+ 1 (car x)) (+ 1 (cdr x))))]))]
       [else
        (stat-add! seen w #t)
        (stat-add! stat w (cons 1 1))]))
     word*)))

;; todo: duplicate
(define (text->vector text)
  (let ([ht (make-hashtable string-hash string=?)]
        [word* (string-split text #\space)])
    (for-each
     (^w (hashtable-set! ht w (+ (hashtable-ref ht w 0) 1)))
     word*)
    ht))

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
