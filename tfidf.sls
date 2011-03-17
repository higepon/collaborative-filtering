;; tfidf
;;
;;   tf : Term Frequency, idf : Inverse Document Frequency
(library (tfidf)
         (export tfidf test)
         (import (rnrs))

(define (tfidf . x)
  x)

(define (test)
  (test-true (analyze '("apple orange" "apple")))


  )





)
