#lang racket

(define (to-number number-func)
  ((number-func (lambda (x) (+ x 1))) 0))

(define (zero-func f)
  (lambda (x) x))
(define zero zero-func)

(define (add-1 number-func)
  (lambda (f)
    (lambda (x)
      (f ((number-func f) x)))))

(define (one-func f)
  (lambda (x) (f x)))
(define one one-func)

(define (two-func f)
  (lambda (x) (f (f x))))
(define two two-func)

(define (add number-func1 number-func2)
  (lambda (f)
    (lambda (x)
      ((number-func1 f) ((number-func2 f) x)))))

(to-number zero)
(to-number one)
(to-number two)