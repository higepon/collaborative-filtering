(library (similarity cosine)
         (export similarity test)
         (import (rnrs)
                 (mosh)
                 (mosh test))

(define (similarity lhs rhs)
  3)

(define (text->vector text)
  '())

(define (test)
  (test-equal '((apple . 1)
                (orange . 2)) (text->vector "orange apple orange"))

  )

)
