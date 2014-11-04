#lang racket

(require "utils.rkt")
(require "math_func.rkt")

(provide equal?)

;(list 'a 'b)
;(car '(ab c))
;(cdr '(a b c))
;(eq? 'a 'a)
;(define x 'a)
;(define y 'a)
;(eq? x y)
;(print x)
;(print y)

(define (memq x seq)
  (cond ((null? seq) false)
        ((eq? (car seq) x) seq)
        (else (memq x (cdr seq)))))

;(list (list 'a))

;(define test '((a b c) (x (y1 y2) z)))
;(print test)
;(car (car test))

;(cadr '((x1 x2) (y1 y2)))

;(pair? '(a short list))

;(print (car ''a))

;(print 'a)
;(print (quote a))
;(print ''a)
;(print (quote (quote a)))
;(print (car (quote (quote a))))

(define (equal? x y)
    (cond ((and (symbol? x) (symbol? y))
           (symbol-equal? x y))
          ((and (list? x) (list? y))
           (list-equal? x y))
          ((and (not (symbol? x)) (not (symbol? y)))
           (= x y))
          (else false)))

(define (symbol-equal? x y)
    (eq? x y))

(define (list-equal? x y)
    (cond ((and (null? x) (null? y)) #t)
          ((or (null? x) (null? y)) #f)
          ((equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
          (else #f)))

;(equal? 1 1)
;(equal? 1 2)
;(equal? 1 'a)
;(equal? 'a 'a)
;(equal? 'a 'b)
;(equal? (list 1 2 3 4) (list 1 2 3 4))
;(equal? (list 1 2 3) (list 1 2 3 4))
;(equal? (list 1 2 3 4) '(a b c d))
;(equal? '(a b c d) '(a b c d))
;(equal? '(a b c d) '(a b c))

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (sum? seq)
  (and (pair? seq) (eq? (car seq) '+)))

(define (make-sum x y)
  ;(list '+ x y)
  (make-sum-imp x y)
  )

(define (make-product x y)
  ;(list '* x y)
  (make-product-imp x y)
  )

(define (make-sum-imp x y)
  (define (inner-numbers a b)
    (+ a b))
  (define (inner-symbols a b)
    (if (eq? a b)
        (list '* 2 b)
        (list '+ a b)))
  (cond ((and (number? x) (number? y)) (inner-numbers x y))
        ((and (symbol? x) (symbol? y)) (inner-symbols x y))
        (else (list '+ x y))))

(define (addend x)
  (car (cdr x)))

(define (augend x)
  (car (cdr (cdr x))))

(define (product? seq)
  (and (pair? seq) (eq? (car seq) '*)))

(define (make-product-imp x y)
  (define (inner-numbers a b)
    (* a b))
  (define (inner-symbols a b)
    (list '* a b))
  (define (inner-number-symbol num sym)
    (cond ((= 0 num) 0)
          ((= 1 num) sym)
          (else (list '* num sym))))
  (cond ((and (number? x) (number? y)) (inner-numbers x y))
        ((and (symbol? x) (symbol? y)) (inner-symbols x y))
        ((and (number? x) (symbol? y)) (inner-number-symbol x y))
        ((and (number? y) (symbol? x)) (inner-number-symbol y x))
        (else (list '* x y))))

(define (multiplicand x)
  (car (cdr x)))

(define (multiplier x)
  (car (cdr (cdr x))))

(define (exponent? seq)
  (and (pair? seq) (eq? (car seq) '**)))

(define (exponent-base seq)
  (car (cdr seq)))

(define (exponent-exp seq)
  (car (cdr (cdr seq))))

(define (make-exponent x y)
  (define (inner-numbers a b)
    (pow a b))
  (define (inner-symbols a b)
    (list '** a b))
  (define (inner-symbol-number a b)
    (cond ((= b 0) 1)
          ((= b 1) a)
          (else (list '** a b))))
  (cond ((and (number? x) (number? y)) (inner-numbers x y))
        ((and (symbol? x) (symbol? y)) (inner-symbols x y))
        (else (list '** x y))))

(define (deriv expression arg)
  (cond ((number? expression) 0)
        ((variable? expression)
         (if (same-variable? expression arg)
             1
             0))
        ((sum? expression)
         (make-sum (deriv (addend expression) arg)
                   (deriv (augend expression) arg)))
        ((product? expression)
         (make-sum
          (make-product (multiplicand expression)
                       (deriv (multiplier expression) arg))
          (make-product (multiplier expression)
                       (deriv (multiplicand expression) arg))))
        ((exponent? expression) ;d(u^n)/dx = n * u^(n-1) * (du/dx)
         (make-product
          (make-product (exponent-exp expression) 
                        (make-exponent
                         (exponent-base expression) 
                         (make-sum (exponent-exp expression) -1)))
          (deriv (exponent-base expression) arg)))
        (else (error "Invalid arg!"))))

;(define test '(* (* x y) (+ x 3)))
;(eq? (car test) '*)
;(deriv test 'x)
;(deriv '(* x (* x x)) 'x)
;(deriv '(** x 3) 'x)

