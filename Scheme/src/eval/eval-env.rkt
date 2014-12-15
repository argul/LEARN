#lang racket
(require scheme/mpair)

(provide variable?)
(provide lookup-variable-value)
(provide extend-env)
(provide declare-variable!)
(provide set-variable-value!)

(define variable? symbol?)

(define (true? x)
  (not (false? x)))

(define (false? x)
  (eq? x 'false))

(define (scan-variable var env proc) ;stupid way, think about generic list operations.
  (define (lookup-inner var var-list value-list)
    (cond ((null? var-list) 'false)
          ((eq? (mcar var-list) var) value-list)
          (else (lookup-inner var (mcdr var-list) (mcdr value-list)))))
  (if (empty-env? env)
      (error "wild variable! " var)
      (let ((result-list (lookup-inner var
                                       (frame-variable (top-frame env)) 
                                       (frame-values (top-frame env)))))
        (if (true? result-list)
            (proc result-list)
            (scan-variable var (parent-env env))))))

(define (lookup-variable-value var env)
  (scan-variable var env (lambda (x) (mcar x))))

(define (extend-env variables values base-env)
  (cons (make-frame variables values) base-env))

(define (declare-variable! var value env)
  (add-bind-to-frame! var value (top-frame env)))

(define (set-variable-value! var value env)
  (scan-variable var env (lambda (x) (set-mcar! x value))))

;begin ENV manipulations
; [ [[var1, var2][value1, value2], [[var1, var2][value1, value2], ....]
(define (parent-env env)
  (mcdr env))

(define (top-frame env)
  (mcar env))

(define empty-frame '())
  
(define (empty-env? env)
  (eq? env empty-frame))

(define (frame-variable frame)
  (mcar frame))

(define (frame-values frame)
  (mcdr frame))

(define (make-frame variables values)
  (cons variables values))

(define (add-bind-to-frame! var value frame)
  (set-mcar! frame (cons var (frame-variable frame)))
  (set-mcdr! frame (cons value (frame-values frame))))
;end ENV