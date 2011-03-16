(library (similarity cosine)
         (export similarity test)
         (import (rnrs)
                 (mosh)
                 (shorten)
                 (mosh control)
                 (mosh test))

(define (similarity lhs rhs)
  3
  )


(define (text->vector text)
  (let ([ht (make-hashtable string-hash string=?)]
        [word* (string-split text #\space)])
    (for-each
     (^w (hashtable-set! ht w (+ (hashtable-ref ht w 0) 1)))
     word*)
    ht))

(define (test)
  (let1 v (text->vector "orange apple orange")
    (test-equal 2 (hashtable-size v))
    (test-equal 1 (hashtable-ref v "apple" 0))
    (test-equal 2 (hashtable-ref v "orange" 0))
    (test-equal 5 (* (norm v) (norm v))))

)



)
