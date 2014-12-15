#lang racket

(require "hash-tab-put-get.rkt")
(require "eval-self-evaluation.rkt")
(require "eval-env.rkt")
(require "eval-quote.rkt")

;begin utils
(define apply-in-underlying-scheme apply)

(define (apply* proc args)
  (define (proc-body proc)
    (cdr proc))
  (define (proc-parameters proc)
    'todo)
  (define (proc-environment proc)
    'todo)
  (cond ((primitive? proc) (apply-primitive-procedure proc args))
        ((compound-proc? proc)
         (eval-sequence (proc-body proc) (extend-env (proc-parameters proc)
                                                     args
                                                     (proc-environment proc))
          ))
        (else (error "Illegal proc"))))

(define (compound-proc? proc)
  'todo)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-ltr exps env)
  (if (null? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (cons left (list-of-values-ltr (rest-operands exps) env)))))

(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-ltr (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) right))))
;end utils

;begin IF
(define (eval-if exp env)
  (define (if-predicate exp)
    (cadr exp))
  (define (if-consequent exp)
    (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
  (if (eval (if-predicate exp) env)
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
;end if

;begin SEQUENCE->EXP
(define (sequence->exp seq)
  'todo)
;end

;begin COND
(define (cond->if exp)
  (expand-clauses (cdr exp)))

(define (else-clause? clause)
  (eq? (car clause) 'else))

(define (suger-clause? clause)
  (and (not (else-clause? clause))
       (eq? (cadr clause) '=>)))

(define (expand-clauses clauses)
  (define (cond-actions cond-exp)
    (cdr cond-exp))
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((else-clause? first) (sequence->exp (cond-actions first)))
              ((suger-clause? first) (make-if (car first)
                                              (sequence->exp (list (caddr first) (car first)))
                                              (expand-clauses rest)))
              (else (make-if (car first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))

(define (eval-cond exp env)
  (eval (cond->if exp) env))
;end cond

;begin SEQUENCE
(define (eval-sequence exp env)
  (define (begin-actions exp) (cdr exp))
  (define (last-exp? seq) (null? (cdr exp)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (let ((exps (begin-actions exp)))
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps) env)))))
;end sequence

;begin ASSIGNMENT
(define (eval-assignment exp env)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
;end assignment

;begin DEFINE
(define (eval-definition exp env)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))
  (define (make-lambda args body)
    (cons 'lambda (cons args body)))
  (declare-variable! (definition-variable exp)
                     (eval (definition-value exp) env)
                     env)
  'ok)
;end define

;begin LAMBDA
(define (make-lambda args body)
  (list 'lambda args body))
(define (eval-lambda exp env)
  'todo)
;end LAMBDA

;begin LET
(define (let-args exp)
  (cadr exp))
(define (let-args-variables args)
  (if (null? args)
      '()
      (cons (caar args) (let-args-variables (cdr args)))))
(define (let-args-values args)
  (if (null? args)
      '()
      (cons (cadr args) (let-args-values (cdr args)))))
(define (let-body exp)
  (caddr exp))
(define (let->lambda exp)
  (let ((args (let-args exp)))
    (cons (make-lambda (let-args-variables args) (let-body exp))
          (let-args-values args))))
(define (eval-let exp env)
  (eval (let->lambda exp) env))
;end let

;begin LET*
(define (make-let args body)
  (list 'let args body))
(define (eval-let* exp env)
  (eval (let*->let (let-args exp) (let-body exp)) env))
(define (let*->let args body)
    (if (null? args)
        body
        (make-let (list (car args)) (let*->let (cdr args) body))))
;end let

;begin PROCEDURE
(define application? pair?)
(define no-operands? null?)
(define (get-operator exp) (car exp))
(define (get-operands exp) (cdr exp))
(define (first-operand exp) (car exp))
(define (rest-operands exp) (cdr exp))

(define (make-procedure args body env)
  (list 'procedure args body env))

(define (compound-procedure? p)
  (eq? 'procedure (car p)))
(define (procedure-args p)
  (cadr p))
(define (procedure-body p)
  (caddr p))
(define (procedure-env p)
  (cadddr p))
;end procedure

;begin PRIMITIVES
(define primitive-table (make-table))
(define (install-primitives)
  (define p 'primitive)
  ((primitive-table 'put) p '+ +)
  ((primitive-table 'put) p '- -)
  ((primitive-table 'put) p '* *)
  ((primitive-table 'put) p '/ /)
  ((primitive-table 'put) p 'car car)
  ((primitive-table 'put) p 'cdr cdr)
  ((primitive-table 'put) p 'list list)
  ((primitive-table 'put) p 'null? null?)
  ((primitive-table 'put) p 'eq? eq?)
  ((primitive-table 'put) p '= =)
  ((primitive-table 'put) p '> >)
  ((primitive-table 'put) p '< <)
  ((primitive-table 'put) p 'not not))
(define (primitive? proc)
  ((primitive-table 'contains) 'primitive proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme ((primitive-table 'get) 'primitive proc) args))
;end PRIMITIVES

;begin install handlers
(define handler-table (make-table))
(define (put-handler op type proc) 
  ((handler-table 'put) op type proc))
(define (get-handler op type)
  ((handler-table 'get) op type))

(put-handler 'op 'quote text-of-quotation)
(put-handler 'op 'set! eval-assignment)
(put-handler 'op 'define eval-definition)
(put-handler 'op 'if eval-if)
(put-handler 'op 'lambda eval-lambda)
(put-handler 'op 'begin eval-sequence)
(put-handler 'op 'cond eval-cond)
(put-handler 'op 'let eval-let)
(put-handler 'op 'let* eval-let*)
;end install handlers

;begin EVAL
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get-handler 'op (car exp)) (apply* (get-handler 'op (car exp)) exp env))
        ((application? exp) (apply* (eval (get-operator exp) env)
                                   (list-of-values (get-operands exp) env)))
        (else (error "Unknown exp type -- EVAL" exp))))
;end EVAL