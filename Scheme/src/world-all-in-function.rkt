#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (zero-func f)
  (lambda (x) x))
(define zero zero-func)

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (one-func f)
  (lambda (x) (f x)))
(define one one-func)

(define (two-func f)
  (lambda (x) (f (f x))))
(define two two-func)

(define (add-func a b)
  )

