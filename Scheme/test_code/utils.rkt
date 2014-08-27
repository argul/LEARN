#lang racket

(provide print)
(provide print2)

(define (print n)
  (display n)(newline))

(define (print2 n m)
  (display n)(display "  ")(display m)(newline))