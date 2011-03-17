;; tfidf
;;
;;   tf : Term Frequency, idf : Inverse Document Frequency
(library (tfidf)
         (export tfidf test)
         (import (rnrs)
                 (shorten)
                 (mosh test))

(define (tfidf . x)
  x)

(define (analyze document*)
  (for-each
   (^d
    (analyze1 '() d)
    )
   document*)
  #f
  )

(define (analyze1 state document)
  #f)

(define (test)
  (test-true (analyze '("apple orange" "apple")))


  )





)
