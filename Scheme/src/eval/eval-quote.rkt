#lang racket

(provide text-of-quotation)

(define (text-of-quotation exp)
  (cadr exp))