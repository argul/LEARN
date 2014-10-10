#lang racket

(define unittest true)

(require "utils.rkt")
(require "math_func.rkt")

(provide accumulate)
(provide integral)
(provide product)

(define (even? x)
  (= 0 (remainder x 2)))

; accumulate all (term min) while (nex min) belows max.
(define (accumulate min max term next)
  ;(accumulate-normal min max term next 0)
  ;(accumulate-tail min max term next 0)
  (accumulate-generalize (lambda (x y) (+ x y)) 0 min max term next (lambda (x) true))
  )

; generic accumulator,
; combine all (term min) approval by filter which (next min) belows max
(define (accumulate-generalize combiner null-value min max term next filter)
  (define (get-filted-value x)
    (if (filter x)
        (combiner null-value x)
        null-value))
  (if (> min max)
      null-value
      (accumulate-generalize combiner                               ;iter-combiner
                             (get-filted-value (term min))          ;iter-null-value
                             (next min)                   
                             max term next filter)))

(define (accumulate-normal min max term next)
  (if (> min max)
      0
      (+ (term min)
         (accumulate-normal (next min) max term next))))

(define (accumulate-tail min max term next sum)
  (if (> min max)
      sum
      (accumulate-tail (next min) max term next (+ sum (term min)))))

(define (accumulate-pi min max)
  (accumulate-generalize (lambda (x y) (+ x y))
                         0 min max
                         (lambda (x) x)
                         (lambda (x) (+ x 1))
                         prime?))

; a + (a + 1) + ...... + (b - 1) + b
(define (acc-integer a b)
  (define (term x)
    x)
  (define (next x)
    (+ x 1))
  (accumulate a b term next))

; a^3 + (a + 1)^3 + ...... + (b - 1)^3 + b^3
(define (acc-cube a b)
  (accumulate a b 
              (lambda (x)
                (* x x x))
              (lambda (x)
                (+ x 1))))

(define (pi-limit steps)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (* 8 (accumulate 1 steps pi-term pi-next)))

; Definite integration of function f(x) from x0 to x1.
(define (integral f x0 x1 steps)
  (if (< steps 1)
      0
      ;(integral-normal f x0 x1 (/ 1.0 steps))
      (integral-simpson f x0 x1 steps)
      ))

(define (integral-normal f x0 x1 dx)
  (accumulate x0 x1 
              (lambda (x)
                (* dx (f x)))
              (lambda (x)
                (+ x dx))))

(define (integral-simpson f x0 x1 steps)
  (define (ensure-even x)
    (if (even? x)
        x
        (+ x 1)))
  (define (ensure-float x)
    (+ x 0.0))
  
  (define x0-real (ensure-float x0))
  (define x1-real (ensure-float x1))
  (define steps-real (ensure-even steps))
  (define dx (/ (- x1-real x0-real) steps-real))
  
  ;h/3 * [y0 + 4*y1 + 2*y2 + 4*y3 + ...... + 2*y(n-2) + 4*y(n-1) + yn]
  ;yk = f(x0 + k * h);
  (define (f-factor step)
    (cond ((= 0 step) 1)
          ((= steps-real step) 1)
          ((even? step) 2)
          (else 4)))
  (define (f-wrapper step)
    (f (+ x0 (* step dx))))
  
  ;body
  (* (/ dx 3.0) (accumulate 0 steps-real (lambda (x) (* (f-factor x) (f-wrapper x))) (lambda (x) (+ x 1)))))

; almost same as accumulate, but do '*' instead of '+'
(define (product min max term next)
  (product-tail min max term next 1))

(define (product-tail min max term next production)
  (if (> min max)
      production
      (product-tail (next min) max term next (* production (term min)))))

(define (pi-product steps)
  (define (ensure-even x)
    (if (even? x) x
        (+ x 1)))
  (define (get-xn n)
    (* 2 (+ 1.0 (/ (ensure-even n) 2))))
  (define (get-yn n)
    (+ 1.0 (* 2 (/ (ensure-even (+ n 1)) 2))))
  ;body
  (* 4 (product 0 steps 
           (lambda (n) (/ (get-xn n) (get-yn n)))
           (lambda (n) (+ 1 n)))))
    

  
;begin unittests
(define (call-unittest)
  (if unittest
      (do-unittest)
      false))

(define (do-unittest)
  (print "1 + 2 + .... + 10 = ")
  (print (acc-integer 1 10))
  (print "1^3 + 2^3 + .... + 10^3 = ")
  (print (acc-cube 1 10))
  (print "1/1*3 + 1/5*7 + 1/9*11 + .... = ")
  (print (pi-limit 100000))
  
  (define (f1 x)
    x)
  (print "integration of f(x) = x from 0 to 1 is ")
  (print (integral f1 0 1 10000))
  
  (define (f2 x)
    (* x x))
  (print "integration of f(x) = x^2 from 0 to 1 is ")
  (print (integral f2 0 1 10000))
  
  (print "1 * 2 * 3 * .... * 10 = ")
  (print (product 1 10 (lambda (x) x) (lambda (x) (+ x 1))))
  
  (print "2*4*4*6*6*8*... /3*3*5*5*7*7*9... is ")
  (print (pi-product 100000))
  
  (print "sum of all primes between 3 to 100")
  (print (accumulate-pi 3 100))
  
  true)

(call-unittest)
;end unittests