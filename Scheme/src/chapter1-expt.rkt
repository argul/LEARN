#lang racket

(require "utils.rkt")

(define (expt1 b n)
  (if (= 0 n)
      1
      (* b (expt1 b (- n 1)))))

;(expt1 2 3)

(define (expt2 b n)
  (define (expt2-iter number exp product)
    (if (= 0 exp)
        product
        (expt2-iter number (- exp 1) (* number product))))
  
  ;body
  (expt2-iter b n 1))

;(expt2 2 10)

;determine if a number is a even
(define (even? n)
  (= (remainder n 2) 0))
  
(define (pow2 x)
  (* x x))

(define (fast-expt number exp)
  (cond ((= exp 0) 1)
        ((even? exp) (fast-expt (pow2 number) (/ exp 2)))
        (else (* number (fast-expt number (- exp 1))))
        )
  )

;(fast-expt 2 10)

(define (fast-expt2 number exp result)
  (cond ((= exp 0) result)
        ((even? exp) (fast-expt2 (pow2 number) (/ exp 2) result))
        (else (fast-expt2 number (- exp 1) (* number result)))
        )
  )

;(fast-expt2 2 10 1)

