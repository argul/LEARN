#lang racket

(require "utils.rkt")

(define (fib n)
  (cond ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (fib (- n 1)) (fib (- n 2)))))
  )

;(fib 10)

(define (fib-common n a b)
  (cond ((= n 0) a)
        ((= n 1) b)
        (else (+ (fib-common (- n 1) a b) (fib-common (- n 2) a b))))
  )

(define (fib-fast n)
  (define (fib-fast-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-fast-iter a b
                          (+ (* q q) (* p p))
                          (+ (* q q) (* 2 p q))
                          (/ count 2)))
          (else (fib-fast-iter (+ (* b q) (* a q) (* a p))
                               (+ (* b p) (* a q))
                               p
                               q
                               (- count 1)))))
  (fib-fast-iter 1 0 0 1 n)
  )

(fib-common 10 0 1)
(fib-fast 10)