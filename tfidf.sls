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

(define (analyze document*)
  (let1 state (make-hashtable string-hash string=?)
    (for-each
     (^d
      (analyze1 state d))
     document*)
    state))

(define (analyze1 state document)
  #f)

(define (test)
  (let1 st (analyze '("apple orange apple" "apple"))
    (test-true st)
    (test-equal '(3 . 2) (hashtable-ref st "apple" #f)))


  )





)
