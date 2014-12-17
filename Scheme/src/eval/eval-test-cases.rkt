#lang racket

(define (test a b) (begin (+ a b) (- a b)))
;(test 1 2)

(let ((a 1) (b 2)) (+ a b))
(let* ((a 1) (b (+ a 1))) (+ a b))

(define mutable 1)
(set! mutable 2)
mutable

(quote (1 2 3))
(if true (display "true") (display "false"))

(define (test-cond a) (cond ((= a 1) (display 1)) ((= a 2) (display 2)) (else (display "unknown"))))

(define (invoke proc) (proc))
(define (test-invoke) (display 1))
(invoke test-invoke)

(define (test-seq) ((display 1) (display 2) (display 3)))
(test-seq)