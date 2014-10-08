#lang racket
(define (pow2 x)
	(* x x))

(define (abs x)
  (if (> x 0) x
      (- x)))

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (sqrt guess x)
  (print guess)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (pow2 guess) x)) 0.001))

(define (average guess x)
  (/ (+ guess x) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(sqrt 1.0 2)