#lang racket

;begin skeleton
(define (mem-proc proc)
  (let ((executed false) (result false))
    (lambda ()
      (if (not executed)
          (begin (set! result (proc))
                 (set! executed true)
                 result)
          result))))

(define (force proc)
  (proc))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (mem-proc (lambda () b))))))

(define (car-stream stream)
  (car stream))

(define (cdr-stream stream)
  (force (cdr stream)))

(define empty-stream '())
(define stream-null? null?)
;end skeleton

;begin test skeleton
;(define test
  ;(cons-stream 1 (cons-stream 2 empty-stream)))
;(car-stream test)
;(cdr-stream test)
;(car-stream (cdr-stream test))
;(cdr-stream (cdr-stream test))
;end test skeleton


;begin functionality
(define (stream-ref stream n)
  (if (= n 0)
      (car-stream stream)
      (stream-ref (cdr-stream stream) (- n 1))))

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (car-stream stream))
             (stream-for-each proc (cdr-stream stream)))))

(define (stream-1-oper oper stream)
  (cons-stream (oper (car-stream stream))
               (stream-1-oper oper (cdr-stream stream))))

(define (stream-2-oper oper s1 s2)
  (cons-stream (oper (car-stream s1) (car-stream s2))
               (stream-2-oper oper (cdr-stream s1) (cdr-stream s2))))

(define (stream-n-oper oper . streams)
  (cons-stream (oper (map car-stream streams))
               (stream-1-oper oper (map cdr-stream streams))))

(define (stream-merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (cons-stream (car-stream s1)
                           (cons-stream (car-stream s2)
                                        (stream-merge (cdr-stream s1) (cdr-stream s2)))))))

(define (stream-merge-comparer comparer s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        ((comparer (car-stream s1) (car-stream s2)) (cons-stream (car-stream s1)
                                                                 (stream-merge-comparer comparer (cdr-stream s1) s2)))
        (else (cons-stream (car-stream s2)
                           (stream-merge-comparer comparer s1 (cdr-stream s2))))))

(define (stream-accumulate stream)
  (define result (cons-stream (car-stream stream)
                              (stream-2-oper + (cdr-stream stream) result)))
  result)

(define (stream-scale factor stream)
  (stream-1-oper (lambda (x) (* x factor)) stream))

(define (stream-head stream n)
  (if (= n 0)
      empty-stream
      (cons-stream (car-stream stream) (stream-head (cdr-stream stream) (- n 1)))))

(define (stream-tail stream n)
  (if (= n 0)
      stream
      (stream-tail (cdr-stream stream) (- n 1))))

(define (display-stream stream)
  (begin (stream-for-each (lambda (x) (begin (display x) (display " "))) stream)
         (display "\n")))

(define (display-head stream n)
  (display-stream (stream-head stream n)))

;end functionality

;begin extensions
(define (make-series-stream initial step-proc)
  (cons-stream initial (make-series-stream (step-proc initial) step-proc)))
;end extensions

;(define (integers-from n)
;  (cons-stream n (integers-from (+ n 1))))
;(define integers (integers-from 1))
;(display-stream (stream-head integers 20))

;begin recursive stream
(define ones (cons-stream 1 ones))
;(display-head ones 10)
(define integers (cons-stream 1 (stream-2-oper + ones integers)))
;(display-head integers 10)
;(display-head (make-series-stream 1 (lambda (x) (* x 2))) 10)

;(display-head (stream-accumulate integers) 10)

;(define s (cons-stream 1 (stream-2-oper + s s)))
;(display-head s 10)
;(define f (cons-stream 1 (stream-2-oper * f (cdr-stream integers))))
;(display-head f 10)
;(display-head (stream-merge ones integers) 20)
;(display-head (stream-merge-comparer < ones integers) 20)
;end recursive stream

(define (integrate-series stream)
  (stream-2-oper * stream (stream-1-oper (lambda (x) (/ 1 x)) integers)))
;(display-head (integrate-series ones) 10)

(define exp-series (cons-stream 1 (integrate-series exp-series)))
;(display-head exp-series 10)

(define cos-series (cons-stream 1 (stream-1-oper (lambda (x) (* -1 x)) (integrate-series sin-series))))
(define sin-series (cons-stream 0 (integrate-series cos-series)))
;(display-head cos-series 10)
;(display-head sin-series 10)
(define (estimate-sin x steps)
  (define x-stream (cons-stream 1 (stream-1-oper (lambda (a) (* a x)) x-stream)))
  ((lambda (stream sum)
    (begin
      (stream-for-each (lambda (a) (set! sum (+ a sum))) stream)
      sum)) (stream-head (stream-2-oper * x-stream sin-series) steps) 0))
;(estimate-sin 1.5 10)

(define (mul-series s1 s2)
  (cons-stream (* (car-stream s1) (car-stream s2))
               (stream-2-oper + (stream-1-oper (lambda (x) (* x (car-stream s1))) (cdr-stream s2))
                              (mul-series (cdr-stream s1) s2))))

;(display-head (stream-2-oper + (mul-series sin-series sin-series) (mul-series cos-series cos-series)) 10)

(define (inverse-series series)
  (define inversed (cons-stream 1 (mul-series 
                                   (stream-1-oper (lambda (x) (* -1 x)) (cdr-stream series))
                                   inversed)))
  (if (not (= 1 (car-stream series)))
      (error "invalid input, first in series must be 1")
      inversed))
;(display-head cos-series 10)
;(display-head (inverse-series cos-series) 10)
;(display-head (mul-series (inverse-series cos-series) cos-series) 10)

(define (div-series s1 s2); return s1 / s2
  (cond ((stream-null? s2) (error "null divisor!"))
        ((stream-null? s1) (error "null dividend!"))
        ((= 0 (car-stream s2)) (error "constant of divisor can't be zero!"))
        ((= 1 (car-stream s2)) (mul-series s1 (inverse-series s2)))
        (else (let ((factor (/ 1 (car-stream s2))))
                (let ((s2-normalize (stream-1-oper (lambda (x) (* x factor)) s2)))
                  (stream-1-oper (lambda (x) (* x factor)) (mul-series s1 (inverse-series s2-normalize))))))))
;(display-head (div-series cos-series cos-series) 10)
;(display-head (div-series cos-series (stream-1-oper (lambda (x) (* x 2)) cos-series)) 10)
;(display-head (div-series sin-series cos-series) 10)

(define (sqrt-stream x)
  (define (sqrt-improve guess x)
    (/ (+ guess (/ x guess)) 2))
  (define guesses (cons-stream 1.0
                               (stream-1-oper (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)
;(display-head (sqrt-stream 2) 10)

(define pi-series (stream-1-oper (lambda (x)
                                   (cond ((= 2 (remainder (- x 1) 4)) (/ -1 x))
                                         ((= 0 (remainder (- x 1) 4)) (/ 1 x))
                                         (else 0)))
                                 integers))
(define pi-stream (stream-scale 4.0 (stream-accumulate pi-series)))
;(stream-ref pi-stream 20)

