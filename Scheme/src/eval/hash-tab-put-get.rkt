#lang racket

(provide put)
(provide get)

(define op-table (make-hash))
(define (put operator type proc)
  (hash-set! op-table (list operator type) proc))
(define (get operator type)
  (hash-ref op-table (list operator type) #f))

(define (attach-tag type contents)
  (cons type contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Invalid datum form!")))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Invalid datum form!")))