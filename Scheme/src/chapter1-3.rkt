#lang racket

(define unittest true)

(require "utils.rkt")
(require "math_const.rkt")
(require "math_func.rkt")

(provide accumulate)
(provide integral)
(provide product)

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

(define (accumulate-prime min max)
  (define (prime-print x)
    (if (prime? x)
      (begin (print x) true)
      false))
  (accumulate-generalize (lambda (x y) (+ x y))
                         0 min max
                         (lambda (x) x)
                         (lambda (x) (+ x 1))
                         ;prime-print
                         prime?
                         ))

(define (accumulate-gcd max)
  (define (gcd-wrapper x)
    (= 1 (gcd x max)))
  (accumulate-generalize (lambda (x y) (* x y))
                         1 1 max
                         (lambda (x) x)
                         (lambda (x) (+ x 1))
                         gcd-wrapper
                         ))

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
    
; find point near where f(x) = 0
(define (seekzero f min max)
  (let ((a (f min))
        (b (f max))
        (tolerate 0.001))
    (cond ((and (negative? a) (positive? b)) (seek-zero-point f min max tolerate))
          ((and (positive? a) (negative? b)) (seek-zero-point f max min tolerate))
          ((= a 0) min)
          ((= b 0) max)
          (else (error "invalid input min and max, not opposite sign")))))
  

(define (seek-zero-point f negative positive tolerate)
  (define (good-enough? negative positive tolerate)
    (< 
     (abs (- negative positive))
     tolerate))
  (let ((middle (average negative positive)))
    (if (good-enough? negative positive tolerate)
      middle
      (let ((fx (f middle)))
      (cond ((= 0 fx) middle)
            ((> fx 0) (seek-zero-point f negative middle tolerate))
            (else (seek-zero-point f middle positive tolerate))
        ))
    )))

(define (seekfix f x)
  (seekfix-imp f x seekfix-tolerate)
  )

(define (seekfix-imp f start-value tolerate)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerate))
  (let ((new-value (f start-value)))
    (if (good-enough? start-value new-value)
        new-value
        (seekfix-imp f (average start-value new-value) tolerate))))

(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) (devired f x)))))

; find x where f(x) = 0 with an initial guess
(define (newton-method f guess)
  (seekfix (newton-transform f) guess))


; fn(1) / fd(1) + (fn(2) / fd(2) + (.....)) until count-k
(define (cont-frac fn fd count-k)
  ;(cont-frac-imp fn fd 1 count-k)
  (cont-frac-imp2 fn fd count-k (+ 0.0 (/ (fn count-k) (fd count-k))))
  )

(define (cont-frac-imp fn fd step count-k)
  (let ((r1 (+ 0.0 (fn step))) (r2 (+ 0.0 (fd step))))
    (if (= count-k step)
        (/ r1 r2)
        (/ r1 (+ r2 (cont-frac-imp fn fd (+ step 1) count-k))))))

(define (cont-frac-imp2 fn fd k sum)
  (let ((r1 (+ 0.0 (fn k))) (r2 (+ 0.0 (fd k))))
    (if (= k 0)
        sum
        (cont-frac-imp2 fn fd (- k 1) (/ r1 (+ r2 sum))))))

(define (math-e-helper k)
  (define n (+ k 1))
  (define r (remainder n 3))
  (if (= 0 r)
      (* 2 (/ r 3))
      1))

(define math-e
  (+ 2 (cont-frac (lambda (x) 1)
                  math-e-helper
                  10000)))
        

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
  (print (accumulate-prime 3 100))
  
  (print "product of all gcd between 1 to 21")
  (print (accumulate-gcd 21))
  
  (print "zero point of f(x) = x")
  (print (seekzero (lambda (x) x) -20.0 10.0))
  
  (print "zero point of f(x) = sin(x)")
  (print (seekzero sin 2.0 4.0))
  
  (print "fix point of f(x) = cos(x)")
  (print (seekfix cos 1.0))
  
  (print "root of function : x = sin(x) + cos(x)")
  (print (seekfix (lambda (x) (+ (sin x) (cos x))) 1.0))
  
  (print "root of function : x = 2.0 / x is sqrt(2)")
  (print (seekfix (lambda (x) (/ 2.0 x)) 1.0))
  
  (print "root of function : x = 1 + 1/x")
  (print (seekfix (lambda (x) (+ 1 (/ 1 x))) 1.0))
  
  (print "root of cont-frac with fn === 1 and fd === 1 is")
  (print (cont-frac (lambda (n) 1) 
                    (lambda (n) 1) 
                    10000))
  
  (print "math-e is")
  (print math-e)
  
  (print "f(x) = x * x * x, f'(x) at x == 1 is")
  (print (devired (lambda (x) (pow x 3)) 1))
  
  (print "try newton method with sqrt2")
  (print (newton-method (lambda (x) (- x (/ 2 x))) 1.0))
  
  ;1.40
  (define (cubic a b c)
    (lambda (x)
      (+ (pow x 3) (* a (pow2 x)) (* b x) c)))
  (print (newton-method (cubic 1 2 3) 1.0))
  
  ;1.41
  (define (double f)
    (lambda (x)
      (f (f x))))
  (define (inc x)
    (+ x 1))
  
  (print ((double inc) 1))
  (print (((double (double double)) inc) 5))
  
  ;1.42
  (define (compose f g)
    (lambda (x) (f (g x))))
  (print ((compose pow2 inc) 6))
  
  ;1.43
  (define (repeated f n)
    (lambda (x)
      (if (= n 1)
          (f x)
          ((compose f (repeated f (- n 1))) x))))

  (print ((repeated pow2 2) 5))
  (print ((repeated (lambda (x) (* 2 x)) 10) 1))
  
  ;1.44
  (define smooth-dx 0.00001)
  (define (smooth f)
    (lambda (x)
      (/ 3 (+ (f (+ x smooth-dx))
              (f x)
              (f (- x smooth-dx))))))
  
  (define (smooth-n f n)
    ((repeated smooth n) f))
  
  (define(test-func x)
    (- (pow x 5) 1))
  (print (((repeated smooth 5) test-func) 2.0))
  (print ((smooth-n test-func 5) 2.0))
  
  ;1.46
  (define (iterative-improve f good-enough? improve)
    (lambda (x)
      (let ((iter (f x)))
        (if (good-enough? x iter)
            iter
            ((iterative-improve f good-enough? improve) (improve x iter))))))
  
  (print ((iterative-improve (lambda (x) (+ 1 (/ 1 x)))
                            (lambda (x x1) (< (abs (- x x1)) seekfix-tolerate))
                            average)
         1.0))
  
  true)

(call-unittest)
;end unittests