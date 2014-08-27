#lang racket

(require "utils.rkt")

(define (sin-iter angle)
  (define (good_enough? t)
    (< t 0.001))
  
  (define (cube t) (* t t t))
  
  (define (calculate t)
    ;(print "calculate is used")
    (- (* 3 t) (* 4 (cube t))))
  
  ;body
  (if (good_enough? angle) 
      angle
      (calculate (sin-iter (/ angle 3)))))

(define (sin x)
  (sin-iter x))

(sin 1.5707)