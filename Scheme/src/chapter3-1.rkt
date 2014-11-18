#lang scheme
(require r5rs)

(define (make-accumulator initial)
  (define (add x)
    (set! initial (+ initial x))
    initial)
  (define (sub x)
    (set! initial (- initial x))
    initial)
  (define (dispatch op x)
    (cond ((equal? op 'add) (add x))
          ((equal? op 'sub) (sub x))
          (else (error "Unknown operator!"))))
  dispatch)

;(define num (make-accumulator 100))
;(num 'add 10)
;(num 'sub 20)

(define (rand-update initial)
  (define (next x)
    (remainder (+ 261 (* 31 x)) 65535))
  (define (generate)
    (set! initial (next initial))
    initial)
  (define (reset x)
    (set! initial x)
    x)
  (define (dispatch op)
    (cond ((equal? op 'generate) generate)
          ((equal? op 'reset) reset)
          (else (error "Unknown operator!"))))
  dispatch)

;(define rnd (rand-update 37))
;((rnd 'generate))
;((rnd 'generate))
;((rnd 'generate))
;((rnd 'reset) 37)
;((rnd 'generate))
;((rnd 'generate))
;((rnd 'generate))

(define test-var-global 1)

(define (test-func2)
  (display test-var-global)
  (display "\n")
  ;(display test-var-inner)
  )

(define (test-func1)
  (define test-var-inner 2)
  (test-func2))

;(test-func1)

(define make-withdraw
  (lambda (balance);evaluated at global env
    (lambda (amount)
      ;evaluated at env in lambda (balance) where balance is binded to input when make-withdraw is called.
      ;every time we enter make-withdraw, an env E is created and holded by the returned func, aka closure.
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!"))))

(define (make-env initial)
  (define balance initial)
  (lambda (op)
    (cond ((equal? op 'add) (lambda (x)
                              (begin (set! balance (+ balance x))
                                     balance)))
          ((equal? op 'sub) (lambda (x)
                              (begin (set! balance (- balance x))
                                     balance)))
          ((equal? op 'reset) (begin (set! balance initial)
                                     balance))
          (else (error "unknown operator!")))))

;(define t1 (make-env 100))
;(define t2 (make-env 100))
;((t1 'add) 10)
;((t2 'add) 10)
;((t1 'sub) 5)
;((t2 'sub) 5)
;(t1 'reset)
;(t2 'reset)

(define (test-env-recursive count)
  (define test 100)
  (if (= 0 count)
      true
      (begin (set! test (- test 10))
             (display test)
             (display "\n")
             (test-env-recursive (- count 1)))))

(define (test-env-recursive2 count initial)
  (define test initial)
  (if (= 0 count)
      true
      (begin (set! test (- test 10))
             (display test)
             (display "\n")
             (test-env-recursive2 (- count 1) test))))

;(test-env-recursive 10)
;(test-env-recursive2 10 100)

;(define test 'test)
;(display test)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;(mystery (list 1 2 3 4))

(define (count-pairs x)
    (length (inner x '())))

(define (inner x memo-list)
    (if (and (pair? x) (false? (memq x memo-list)))
        (inner (car x) (inner (cdr x) (cons x memo-list)))
        memo-list))