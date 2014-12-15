#lang racket

(provide variable?)
(provide lookup-variable-value)
(provide extend-env)
(provide declare-variable!)
(provide set-variable-value!)

(define variable? symbol?)
(define (lookup-variable-value exp env)
  'todo)

(define (extend-env variables values base-env)
  'todo)

(define (declare-variable! var value env)
  'todo)

(define (set-variable-value! var value env)
  'todo)