#lang racket

(require "utils.rkt")
(require "math_func.rkt")
(require "symbol.rkt")

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum")))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum")))

(define (apply-generic op . args)
  (define type-tags (map type-tag args))
  (define proc (get op type-tags))
  (if proc
      (apply proc (map contents args))
      (error "No method for types : " (list op type-tags))))

(define (add x y) (apply-generic 'add x y))