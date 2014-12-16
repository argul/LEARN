#lang racket

(define (print . args)
  (define (print-inner list)
    (if (null? list)
        (display "\n")
        (begin (display (car list))
               (display " ")
               (print-inner (cdr list)))))
  (print-inner args))

;region CORE
(define (eval exp env)
  (cond ((self-evaluating? exp) exp);; 1 => 1
        ((variable? exp) (seek-variable exp env));; a => seek value binded to 'a' in env
        ((contains-handler (car exp)) ((get-handler (car exp)) exp env));; built-in keywords/pattens
        ((application? exp) (apply* (eval (get-operator exp) env);; procedure invocation (a b c) => (apply a (b c)) 
                                    (eval-args (get-args exp) env)))
        (else (error "Unknown exp type -- EVAL" exp))))

(define (apply* proc args)
  ;(print "apply*" proc args)
  (cond ((primitive? proc) (apply-primitive-procedure proc args))
        ((compound-proc? proc) ; eval-sequence body here is the most critical word.
         (eval-sequence (proc-body proc) (extend-env (proc-parameters proc)
                                                     args
                                                     (proc-environment proc))))
        (else (error "Illegal proc"))))

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
;end region

;region symbol
(define variable? symbol?)
;end region

;regin quote
(define (eval-quote exp env)
  (cadr exp))
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

(define (eval-if exp env)
  (if (eval (if-predicate exp) env)
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
;end region

;region SEQUENCE
(define (make-begin seq) (list 'begin seq))
(define (get-sequence exp)
  (cdr exp))
(define (last-item? seq) (null? (cdr seq)))
(define (first-item seq) (car seq))
(define (rest-seq seq) (cdr seq))

(define (eval-sequence seq env)
  (cond ((last-item? seq) (eval (first-item seq) env))
        (else (eval (first-item seq) env)
              (eval-sequence (rest-seq seq) env))))

(define (eval-begin exp env)
  (eval-sequence (get-sequence exp) env))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-item? seq) (first-item seq))
        (else (make-begin seq))))
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

(define (eval-cond exp env)
  (eval (cond->if exp) env))
;end region

;region ASSIGNMENT
(define (eval-assignment exp env)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
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
(define (eval-definition exp env)
  (set-variable-value! (definition-variable exp)
                       (eval (definition-value exp) env) ; eval-lambda
                       env)
  'ok)
;end region

;region LAMBDA
(define (make-lambda args body)
  (list 'lambda args body))
; ['lambda (a b) (+ a b)]
(define (lambda-args exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (eval-lambda exp env)
  (make-procedure (lambda-args exp)
                  (lambda-body exp)
                  env))
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
(define (eval-let exp env)
  (eval (let->lambda exp) env))
;end region

;region LET*
(define (make-let args body)
  (list 'let args body))
(define (eval-let* exp env)
  (eval (let*->let (let-args exp) (let-body exp)) env))
(define (let*->let args body)
  (if (null? args)
      body
      (make-let (list (car args)) (let*->let (cdr args) body))))
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
(put-handler 'quote eval-quote)
(put-handler 'set! eval-assignment)
(put-handler 'define eval-definition)
(put-handler 'if eval-if)
(put-handler 'lambda eval-lambda)
(put-handler 'begin eval-begin)
(put-handler 'cond eval-cond)
(put-handler 'let eval-let)
(put-handler 'let* eval-let*)
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