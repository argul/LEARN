#lang racket

(require "utils.rkt")
(require "math_const.rkt")
(require "math_func.rkt")

(define (to-float number)
  (+ (0.0 number)))

(define (make-interval min max)
  (cons min max))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-interval2 origin deviation)
  (if (< deviation 0)
      (error "deviation can't be negative!")
      (make-interval-imp origin (to-float deviation))))

(define (make-interval2-imp origin deviation)
  (let ((diff (/ deviation 2)))
    (make-interval (- origin diff) (+ origin diff))))

(define (add-interval a b)
  ())