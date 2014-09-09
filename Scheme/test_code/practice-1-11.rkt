#lang racket

(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3)))
       )))

(define (fmla x p1 p2 p3)
  (if (< x 3)
      x
      (+ p1 (* 2 p2) (* 3 p3))))

(define (g n)
  (define (g-inner n count p1 p2 p3)
    (if (= n count)
        p1
        (g-inner n (+ count 1) (fmla (+ count 1) p1 p2 p3) p1 p2)
        ))
  (g-inner n 2 2 1 0))

(define (tail-recursive fn n initials)
  (define (length list c)
    (if (null? list)
        c
        (length (cdr list) (+ 1 c))))
  
  (define (get-index n array count)
    (if (= n count)
        (car array)
        (get-index n (cdr array) (+ 1 count))))
  
  (define (next)
    (fn initials))
  
  (define (step-forward array)
    (cons (next) (cdr array)))
  
  (define (inner count array)
    (if (= n count)
        (car array)
        (inner (+ count 1) (step-forward array))))
  (inner (length initials 0) initials))

(define (fib last-two)
  (+ (car last-two (car (cdr last-two)))))

(tail-recursive fib 3 (make-list 1 2))