#lang racket

(require "../utils.rkt")

;region CORE
(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((contains-handler (car exp)) ((get-handler (car exp)) exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown exp type -- EVAL" exp))))

;; (a b c) => (apply a (b c))
(define (get-operator exp)
  (car exp))
(define (get-args exp)
  (cdr exp))

;; (define (add a b) (+ a b)) => ['procedure (a b) (+ a b) env]
(define (make-procedure args body env)
  (list 'procedure args body env))

(define (compound-proc? exp)
  (if (pair? exp)
      (eq? (car exp) 'procedure)
      false))

(define (proc-parameters proc)
  (cadr proc))
(define (proc-body proc)
  (caddr proc))
(define (proc-environment proc)
  (cadddr proc))
(define application? pair?)

(define (eval-args args env)
  (if (null? args)
      '()
      (cons (eval (car args) env)
            (eval-args (cdr args) env))))


(define (analyze-application exp)
  (let ((opproc (analyze (get-operator exp)))
        (argproc-list (map analyze (get-args exp))))
    (lambda (env)
      (execute-application (opproc env)
                           (map (lambda (proc) (proc env)) argproc-list)))))

(define (execute-application proc args)
  (cond ((primitive? proc) (apply-primitive-procedure proc args))
        ((compound-proc? proc)
         (print proc)
         ((proc-body proc) (extend-env (proc-parameters proc)
                                       args
                                       (proc-environment proc))))))
;end region

;regin HANDLERS DICTIONARY
(define (make-table)
  (let ((tb (make-hash)))
    (define (put type proc)
      (hash-set! tb type proc))
    (define (get type)
      (hash-ref tb type #f))
    (define (contains type)
      (not (eq? (get type) #f)))
    (define (remove type)
      (hash-remove! tb type))
    (lambda (code)
      (cond ((eq? code 'put) put)
            ((eq? code 'get) get)
            ((eq? code 'contains) contains)
            ((eq? code 'remove) remove)))))

(define handler-table (make-table))
(define (put-handler type proc) 
  ((handler-table 'put) type proc))
(define (get-handler type)
  ((handler-table 'get) type))
(define (contains-handler type)
  ((handler-table 'contains) type))
;end region

;region ENVIRONMENT
(define empty-env-stack '())
(define (parent-env-stack env-stack) (cdr env-stack))
(define (top-env env-stack) (car env-stack))

(define (make-env var-list value-list)
  (define (bind table var value)
    ((table 'put) var value))
  (define (bind-list table var-list value-list)
    (if (null? var-list)
        'ok
        (begin
          (bind table (car var-list) (car value-list))
          (bind-list table (cdr var-list) (cdr value-list)))))
  (let ((t (make-table)))
    (bind-list t var-list value-list)
    t))

(define (add-binding! env var value)
  ((env 'put) var value))

(define (remove-binding! env var)
  ((env 'remove) var))

(define (extend-env var-list value-list env-stack)
  (cons (make-env var-list value-list) env-stack))

;; proc is (lambda (value corresponding-env) body)
(define (scan-variable var env-stack proc)
  (if (null? env-stack)
      #f
      (let ((env (top-env env-stack)))
        (if ((env 'contains) var)
            (let ((value ((env 'get) var)))
              (begin (proc value env) value))
            (scan-variable var (parent-env-stack env-stack) proc)))))

(define (seek-variable var env-stack)
  (let ((ret (scan-variable var env-stack (lambda (value env) value))))
    (if (eq? #f ret)
        (error "Unbinded variable" var)
        ret)))

(define (set-variable-value! var new-value env-stack)
  (if (eq? #f (scan-variable var env-stack (lambda (old-value env)
                                             (add-binding! env var new-value))))
      (begin (add-binding! (top-env env-stack) var new-value) 'ok)
      'ok))
;end region

;regin SELF-EVALUATING
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))
;end region

;region symbol
(define variable? symbol?)
(define (analyze-variable exp)
  (lambda (env) (seek-variable exp env)))
;end region

;regin quote
(define (analyze-quote exp)
  (let ((qval (cadr exp)))
    (lambda (exp) qval)))
;end region

;region IF
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (pproc env)
          (cproc env)
          (aproc env)))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
;end region

;region SEQUENCE
(define (analyze-sequence exp-list)
  (define (make-lambda-sequence proc1 proc2)
    (lambda (env)
      (proc1 env)
      (proc2 env)))
  (define (make-lambda-loop first rest)
    (if (null? rest)
        first
        (make-lambda-loop (make-lambda-sequence first (car rest)) (cdr rest))
        ;(make-lambda-sequence first (make-lambda-loop (car rest) (cdr rest)))
        ))
  (let ((proc-list (map analyze exp-list)))
    (make-lambda-loop (car proc-list) (cdr proc-list))))

(define (make-begin seq) (list 'begin seq))
(define (last-item? seq) (null? (cdr seq)))
(define (first-item seq) (car seq))
(define (get-sequence exp)
  (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-item? seq) (first-item seq))
        (else (make-begin seq))))

(define (analyze-begin exp)
  (analyze-sequence (get-sequence exp)))
;end region

;region COND
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

(define (analyze-cond exp)
  (analyze (cond->if exp)))
;end region

;region ASSIGNMENT
(define (analyze-assignment exp)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (let ((var (assignment-variable exp))
        (value-proc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (value-proc env) env)
      'ok)))
;end region

;region DEFINE
; (define a b)
; (define (add a b) (+ a b)) => (define add (lambda (a b) (+ a b)))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (caddr exp))))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (value-proc (analyze (definition-value exp))))
    (lambda (env)
      (set-variable-value! var (value-proc env) env)
      'ok)))

;end region

;region LAMBDA
(define (make-lambda args body)
  (list 'lambda args body))
; ['lambda (a b) (+ a b)]
(define (lambda-args exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (analyze-lambda exp)
  (let ((vars (lambda-args exp))
        (proc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars proc env))))
;end region

;region LET
(define (let-args exp)
  (cadr exp))
(define (let-args-variables args)
  (if (null? args)
      '()
      (cons (caar args) (let-args-variables (cdr args)))))
(define (let-args-values args)
  (if (null? args)
      '()
      (cons (cadar args) (let-args-values (cdr args)))))
(define (let-body exp)
  (caddr exp))
(define (let->lambda exp)
  (let ((args (let-args exp)))
    (cons (make-lambda (let-args-variables args) (let-body exp))
          (let-args-values args))))
(define (analyze-let exp)
  (analyze (let->lambda exp)))
;end region

;region LET*
(define (make-let args body)
  (list 'let args body))
(define (let*->let args body)
  (if (null? args)
      body
      (make-let (list (car args)) (let*->let (cdr args) body))))
(define (analyze-let* exp)
  (analyze (let*->let (let-args exp) (let-body exp))))
;end region

;region PRIMITIVES
(define (primitive? proc)
  (eq? (car proc) 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args); + = ['primitive +]
  (apply (primitive-implementation proc) args))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'display display)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
;end region

;region REGISTER HANDLERS
(put-handler 'quote analyze-quote)
(put-handler 'set! analyze-assignment)
(put-handler 'define analyze-definition)
(put-handler 'if analyze-if)
(put-handler 'lambda analyze-lambda)
(put-handler 'begin analyze-begin)
(put-handler 'cond analyze-cond)
(put-handler 'let analyze-let)
(put-handler 'let* analyze-let*)
;end region

;region INITIAL ENVIRONMENT
(define (setup-environment)
  (let ((initial-env
         (extend-env (primitive-procedure-names)
                     (primitive-procedure-objects)
                     empty-env-stack)))
    (add-binding! (top-env initial-env) 'true true)
    (add-binding! (top-env initial-env) 'false false)
    initial-env))
;end region

;region INTERACTION LOOP
(define input-prompt " mscheme > ")
(define output-prompt " .. ")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-proc? object)
      (display (list 'compound-procedure
                     (proc-parameters object)
                     (proc-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
(driver-loop)
;end region