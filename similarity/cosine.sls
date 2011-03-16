(library (similarity cosine)
         (export similarity test)
         (import (rnrs)
                 (mosh)
                 (shorten)
                 (mosh control)
                 (mosh test))

(define (similarity lhs rhs)
  (let ([lvec (text->vector lhs)]
        [rvec (text->vector rhs)])
    (write (hashtable-size lvec))
    (newline)
    (write (hashtable-size rvec))
    (newline)
    (/ (innter-product lvec rvec) (* (norm lvec) (norm rvec)))))

(define (hashtable-fold-left proc seed ht)
  (let* ([key* (hashtable-keys ht)]
         [len (vector-length key*)])
    (let loop ([i 0]
               [ret seed])
      (cond
       [(= len i) ret]
       [else
        (loop (+ i 1)
              (proc ret (vector-ref key* i) (hashtable-ref ht (vector-ref key* i))))]))))

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
  (let1 v (text->vector "orange apple orange")
    (test-equal 2 (hashtable-size v))
    (test-equal 1 (hashtable-ref v "apple" 0))
    (test-equal 2 (hashtable-ref v "orange" 0))
    (test-true (good-enough? 2.2360 (norm v))))
)

;; todo: move
(define (good-enough? x y)
    ;; relative error should be with 0.1%, but greater
    ;; relative error is allowed when the expected value
    ;; is near zero.
    (cond ((not (number? x)) #f)
          ((not (number? y)) #f)
          ((or (not (real? x))
               (not (real? y)))
           (and (good-enough? (real-part x) (real-part y))
                (good-enough? (imag-part x) (imag-part y))))
          ((infinite? x)
           (= x (* 2.0 y)))
          ((infinite? y)
           (= (* 2.0 x) y))
          ((nan? y)
           (nan? x))
          ((> (magnitude y) 1e-6)
           (< (/ (magnitude (- x y))
                 (magnitude y))
              1e-3))
          (else
           (< (magnitude (- x y)) 1e-6))))



)
