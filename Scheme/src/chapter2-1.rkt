#lang racket

(require "utils.rkt")
(require "math_const.rkt")
(require "math_func.rkt")

(define unit-test true)

(define (cons-special x y)
  (define (dispatch m)
    (if (= m 0)
        x
        y))
  dispatch)

(define (car-special r)
  (r 0))

(define (cdr-special r)
  (r 1))

(define (cons-special2 x y)
  (lambda (m) (m x y)))

(define (car-special2 r)
  (r (lambda (x y) x)))

(define (cdr-special2 r)
  (r (lambda (x y) y)))


(define (sign x)
  (if (> x 0) 1 -1))

(define (cons-special3 x y)
  (* (sign x) (pow 2 (abs x)) (pow 3 y)))

(define (decompose product factor per-tick-func initial)
  (decompose-imp product factor per-tick-func initial))

(define (decompose-imp product factor per-tick-func result)
  (if (= 0 (remainder product factor))
      (decompose-imp (/ product factor) factor per-tick-func (per-tick-func result))
      result))

(define (car-special3 r)
  (* (sign r) (decompose (abs r) 2 (lambda (x) (+ x 1)) 0)))

(define (cdr-special3 r)
  (decompose (abs r) 3 (lambda (x) (+ x 1)) 0))

(define cons-imp cons)
(define car-imp car)
(define cdr-imp cdr)

;REALLY AWESOME!
;(define cons-imp cons-special)
;(define car-imp car-special)
;(define cdr-imp cdr-special)

;(define cons-imp cons-special2)
;(define car-imp car-special2)
;(define cdr-imp cdr-special2)

;(define cons-imp cons-special3)
;(define car-imp car-special3)
;(define cdr-imp cdr-special3)


(define (make-rat n d)
  (if (or (= d 0) (= n 0))
      (error "invalid args! numer or denom can't be zero!")
      (let ((g (gcd n d)))
        (make-rat-imp (/ n g) (/ d g)))))

(define (make-rat-imp n d)
  (cond ((and (< n 0) (< d 0)) (cons-imp (* -1 n) (* -1 d)))           ; -2/-5 = 2/5
        ((and (> n 0) (< d 0)) (cons-imp (* -1 n) (* -1 d)))           ; 2/-5 = -2/5
        (else (cons-imp n d))))                                        ; 2/5

(define (numer x)
  (car-imp x))

(define (denom x)
  (cdr-imp x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat r)
  (display (numer r))
  (display "/")
  (display (denom r))
  (display "\n"))

(define (rat-to-decimal r)
  (/ (+ 0.0 (numer r)) (denom r)))


(define zero
  (lambda (whatever)
    (lambda (itself) itself)))

(define (add-1 number-proc)
  (lambda (operatee) (lambda (x) (operatee ((number-proc operatee) x)))))


(define (call-unit-test)
  (if unit-test 
      (do-unit-test)
      false))

(define (do-unit-test)
  (define r (make-rat 2 5))
  (print-rat r)
  (print (numer r))
  (print (denom r))
  
  (print-rat (make-rat -2 -5))
  (print-rat (make-rat 2 -5))
  (print-rat (make-rat 10 25))
  
  true)

(call-unit-test)