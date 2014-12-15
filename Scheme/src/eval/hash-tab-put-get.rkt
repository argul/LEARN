#lang racket

(provide make-table)

(define (make-table)
  (let ((tb (make-hash)))
    (define (put operator type proc)
      (hash-set! tb (list operator type) proc))
    (define (get operator type)
      (hash-ref tb (list operator type) #f))
    (define (contains operator type)
      (not (eq? (get operator type) #f)))
    (lambda (code)
      (cond ((eq? code 'put) put)
            ((eq? code 'get) get)
            ((eq? code 'contains) contains)))))

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