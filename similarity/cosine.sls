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

(define (norm v)
  (sqrt (hashtable-fold-left
         (^(seed key value)
           (+ seed (* value value)))
         0 v)))

(define (innter-product lhs rhs)
  (hashtable-fold-left
   (^(accum key lvalue)
     (cond
      [(hashtable-ref rhs key #f) =>
       (^(rvalue)
         (+ accum (*  rvalue lvalue)))]
      [else accum]))
   0.0 lhs))

(define (test)
  (define vec1 '(("America" . 1) ("Canada" . 2) ("Japan" . 1)))
  (define vec2 '(("America" . 1) ("Korea" . 1)))
  (define vec3 '(("America" . 1) ("Canada" . 2) ("Korea" . 1)))

  (test-equal 6.0 (innter-product (alist->string-hashtable vec1) (alist->string-hashtable vec1)))
  (test-equal 1.0 (innter-product (alist->string-hashtable vec1) (alist->string-hashtable vec2)))
  (test-equal 2.0 (innter-product (alist->string-hashtable vec2) (alist->string-hashtable vec3)))
  (test-equal 5.0 (innter-product (alist->string-hashtable vec1) (alist->string-hashtable vec3)))
  (test-true (good-enough? 1.0 (similarity vec1 vec1)))
  (test-true (< (similarity vec1 vec2) (similarity vec1 vec3)))
  (test-true (< (similarity vec1 vec2) (similarity vec2 vec3)))
)

)
