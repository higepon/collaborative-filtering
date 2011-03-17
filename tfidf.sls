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
  (test-true (analyze '("apple orange" "apple")))


  )





)
