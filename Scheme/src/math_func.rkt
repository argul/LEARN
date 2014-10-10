#lang racket

(require "utils.rkt")
(require "math_const.rkt")

(provide abs)
(provide average)
(provide max)
(provide min)
(provide pow)
(provide pow2)
(provide sqrt)
(provide factorial)
(provide expt)
(provide fib)
(provide sin)
(provide cos)
(provide tan)
(provide ctan)
(provide even?)
(provide gcd)
(provide prime?)

;/*abs*/

(define (abs x)
  (if (> x 0)
      x
      (- x))
  )

;/*average*/
(define (average x y)
  (/ (+ x y) 2))

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
  (sqrt-inner 1.0 x sqrt-threshold)
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

;/*trigonometric*/
(define (sin x)
  ;begin fmla
  (define (fmla x)
    (- (* 3 x) (* 4 (pow x 3))))
  ;end fmla
  ;begin inner
  (define (inner x threshold)
    (if (not (> (abs x) threshold))
        x
        (fmla (/ x 3))))
  ;end inner
  (inner x sin-threshold))

(define (cos x)
  (sqrt (- 1 (pow2 (sin x)))))

;TODO : divide by zero
(define (tan x)
  (/ (sin x) (cos x)))

(define (ctan x)
  (/ (cos x) (sin x)))

;/*GCD*/
(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

;/*prime*/
(define (prime? n)
  (fermat-prime? n fermat-test-times)
  )

(define (fermat-prime? n times)
  (define (real-times)
    (if (> times (- n 2)) (- n 2)
        times))
  (cond ((< n 2) false)
        ((= n 2) true)
        ((= times 0) true)
        ((fermat-check? n) (fermat-prime? n (- (real-times) 1)))
        (else false)))
        

(define (fermat-check? n)
  (define (check-number a)
    (= a (remainder (pow-restrict a n n) n))) ;fermat thoery
  (define rnd (rnd-1-n n))
  (if (nontrivial-root? rnd n) (fermat-check? n)
      (check-number rnd)))

(define (rnd-1-n n)
  (+ 1 (random (- n 1))))

;check if 1 == ((a * a) mod n)
(define (nontrivial-root? a n)
  (cond ((= a 1) false)
        ((= a (- n 1)) false)
        (else (= 1 (remainder (pow2 a) n)))))

;prepare to calculate (pow a n) mod n, but since
;((a + b * n) * c) mod n == (a * c) mod n, we just skip
;the unnecessary mutiply.
(define (pow-restrict base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (pow2 (pow-restrict base (/ exp 2) m)) m))
        (else
         (remainder (* base (pow-restrict base (- exp 1) m)) m))))

;/*unit test of prime*/
(define (test-prime rnd)
  (print rnd)
  (if (prime? rnd)
      (print "true")
      (print "false")))

(define (test-runtime-prime n)
  (cond ((= 0 n) true)
        (else (test-prime (+ 1 (* 2 (random 1000))))
              (test-runtime-prime (- n 1)))
        ))


;(test-runtime-prime 20)