#lang racket

(require "utils.rkt")
(require "math_func.rkt")
(require "symbol.rkt")

; meta rules
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) 
      set
      (cons x set)))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (intersection-set (cdr set1) set2))
        (else (cons (car set1) (intersection-set (cdr set1) set2)))))


(define (intersection-sorted-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2)) (cons (car set1) (intersection-sorted-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (intersection-sorted-set (cdr set1) set2))
        (else (intersection-sorted-set (cdr set2) set1))))

(define (union-sorted-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-sorted-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-sorted-set (cdr set1) set2)))
        (else (cons (car set2) (union-sorted-set (cdr set2) set1)))))