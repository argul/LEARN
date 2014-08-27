#lang racket

(require "utils.rkt")

(provide pow)
(provide pow2)


(define (pow-inner x n product)
  (cond ((= n 0) product)
        ((even? n) (pow-inner (* x x) (/ n 2) product))
        (else (pow-inner x (- n 1) (* x product)))
      )
  )

(define (pow x n)
  (pow-inner x n 1)
  )

(define (pow2 x)
  (pow x 2))
  
  

(define (sqrt-inner guess x tolerate)
  (define (good-enough? guess x)
    (< (abs (- (pow2 guess) x)) tolerate))
  (define (average guess x)
    (/ (+ guess x) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  
  (if (good-enough? guess x)
      guess
      (sqrt-inner (improve guess x) x tolerate))
  )

(define (sqrt x)
  (sqrt-inner 1.0 x 0.0001)
  )