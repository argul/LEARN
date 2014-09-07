#lang racket

(require "utils.rkt")

(provide abs)
(provide max)
(provide min)
(provide pow)
(provide pow2)
(provide sqrt)
(provide factorial)
(provide expt)
(provide fib)

;/*abs*/

(define (abs x)
  (if (> x 0)
      x
      (- x))
  )

;/*pow*/

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

;/*sqrt*/

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

;/*operator2*/

(define (calc oper? a b)
  (if (oper? a b)
      a
      b))

(define (max a b)
  (calc > a b))

(define (min a b)
  (calc < a b))

(define (practice-1-3 a b c)
  (- (+ a b c) (min (min a b) c))
  )

(define (practice-1-4 a b)
  ((if (> b 0)
       +
       -) a b))

;(practice-1-3 1 2 3)
;(practice-1-4 2 -3)

;/*factorial*/
(define (factorial1 n)
  (if (<= n 1)
      1
      (* n (factorial1 (- n 1)))
      ))

;(factorial1 4)

(define (factorial n)
  (factorial-iter 1 1 n))

(define (factorial-iter count product max)
  (if (> count max)
      product
      (factorial-iter (+ count 1) (* count product) max)
      ))

;(factorial 4)

(define (Ackermann x y)
  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)
                   (A x (- y 1))))
          ))
  (A x y))

(define (f n) (Ackermann 0 n))
(define (g n) (Ackermann 1 n))
(define (h n) (Ackermann 2 n))

(define (practice-1-10 proc)
  (print (proc 1))
  (print (proc 2))
  (print (proc 3))
  (print (proc 4))
  (print (proc 5)))

;(practice-1-10 f)
;(practice-1-10 g)
;(practice-1-10 h)

;/*fib*/
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

(define (fib n)
  (fib-fast n))

;/*expt*/
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt number exp)
  (cond ((= exp 0) 1)
        ((even? exp) (fast-expt (pow2 number) (/ exp 2)))
        (else (* number (fast-expt number (- exp 1))))
        )
  )

(define (fast-expt2 number exp result)
  (cond ((= exp 0) result)
        ((even? exp) (fast-expt2 (pow2 number) (/ exp 2) result))
        (else (fast-expt2 number (- exp 1) (* number result)))
        )
  )

(define (expt x n)
  (fast-expt2 x n 1))
