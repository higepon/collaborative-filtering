(library (similarity cosine)
         (export similarity test)
         (import (rnrs)
                 (mosh)
                 (shorten)
                 (mosh control)
                 (mosh test))

(define (alist->string-hashtable alist)
  (let ([hashtable (make-hashtable string-hash string=?)])
    (for-each (lambda (x) (hashtable-set! hashtable (car x) (cdr x)))
              alist)
    hashtable))

(define (similarity lhs rhs)
  (let ([lvec (alist->string-hashtable lhs)]
        [rvec (alist->string-hashtable rhs)])
    (/ (innter-product lvec rvec) (* (norm lvec) (norm rvec)))))

(define (text->vector text)
  (let ([ht (make-hashtable string-hash string=?)]
        [word* (string-split text #\space)])
    (for-each
     (^w (hashtable-set! ht w (+ (hashtable-ref ht w 0) 1)))
     word*)
    ht))

(define (norm v)
  (sqrt (hashtable-fold-left
         (^(seed key value)
           (+ seed (* value value)))
         0 v)))

(define (innter-product lhs rhs)
  (let loop ([key* (vector->list (hashtable-keys lhs))]
             [ret 0.0])
    (cond
     [(null? key*) ret]
     [(hashtable-ref rhs (car key*) #f) =>
      (^(value)
;        (format #t "key=~a (* ~a ~a) = ~a\n" (car key*) (hashtable-ref lhs (car key*) #f) value (* (hashtable-ref lhs (car key*) #f) value))
        (loop (cdr key*)
              (+ ret (* (hashtable-ref lhs (car key*) #f) value))))]
     [else
      (loop (cdr key*) ret)])))

(define (test)
  (define vec1 '(("America" . 1) ("Canada" . 2) ("Japan" . 1)))
  (define vec2 '(("America" . 1) ("Korea" . 1)))
  (define vec3 '(("America" . 1) ("Canada" . 2) ("Korea" . 1)))

  (test-true (good-enough? 1.0 (similarity vec1 vec1)))
  (test-true (< (similarity vec1 vec2) (similarity vec1 vec3)))
  (test-true (< (similarity vec1 vec2) (similarity vec2 vec3)))
)

)
