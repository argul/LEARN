#lang racket

(require "utils.rkt")
(require "math_func.rkt")

(define (g . w)
  w)

(define k
  (lambda w w))

(define (add2 . w)
  (add2-imp w))
  
(define (add2-imp list)
  (if (null? list)
      0
      (+ (car list) (add2-imp (cdr list)))))

;(map add2 (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))
;(map + (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))

(define (map2 proc . items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map2 proc (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (append2 items element)
  (append items (list element)))

(define (reverse items)
  (if (null? items)
      null
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (reverse (map reverse items)))

(define (square-list2 items answer)
  (define (square x)
    (* x x))
  (if (null? items)
      answer
      (square-list2 (cdr items) (append2 answer (square (car items))))))

(define test (list 1 (list 2 (list 3 4) (list 5 6)) (list (list 7 8 9) 10)))

(define (tree-map proc tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map proc x)
             (proc x))) tree))

;(square-list (list 2 4 5 6 7))
;(square-list2 (list 2 4 5 6 7) null)
;(append (list 1 2 3 4) (list 5 6 7 8))
;(reverse (append (list 1 2 3 4) (list 5 6 7 8)))
;(cons (list 1 2) (list 3 4))

;(reverse test)
;(deep-reverse test)
;(tree-map (lambda (x) (* x x)) test)

(define (subset set)
  (if (null? set)
      (list null)
      (let ((head (car set)) (rest (subset (cdr set))))
        (append rest (map (lambda (x) (cons head x)) rest)))))


(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;(filter (lambda (x) (= 0 (remainder x 2))) (list 1 2 3 4 5 6 7))

(define (accumulate operator initial sequence)
  (if (null? sequence)
      initial
      (operator (car sequence) (accumulate operator initial (cdr sequence)))))

;(accumulate + 0 (list 1 2 3 4 5))

(define (enumrate-interval min max step)
  (if (> min max)
      null
      (cons min (enumrate-interval (+ min step) max step))))

;(enumrate-interval 1 100 2)

(define (map3 proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y)) '() sequence))

;(map3 (lambda (x) (* x x)) (list 1 2 3 4))

; an*x^n + a(n-1)*x^(n-1) + ...... + a1*x + a0
(define (horner-eval x sequence a0)
  (accumulate (lambda (an result) (+ (* result x) an)) a0 sequence))

;(horner-eval 2 (list 1 0 5 0 3) 1)

(define (count-leaves sequence)
  (accumulate (lambda (current subsequence)
                (print "--------------------------")
                (print current)
                (print subsequence)
                (print "--------------------------")
                (if (pair? current)
                    (append (count-leaves current) subsequence)
                    (cons current subsequence))) 
              '() sequence))

(print test)
(count-leaves test)

(define (accumulate-n operator init sequences)
  (if (null? (car sequences))
      '()
      (cons (accumulate operator init (map car sequences))
            (accumulate-n operator init (map cdr sequences)))))

(list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (dot-product vector1 vector2)
  (if (not (= (length vector1) (length vector2)))
      (error "invalid arg! length mismatch!")
      (accumulate + 0 (map * vector1 vector2))))

(dot-product (list 1 2 3) (list 4 5 6))

(define (make-matrix . vectors)
  (cond ((null? vectors) (error "input is null!"))
        ((not (pair? (car vectors))) (error "invalid inputs! not pair!"))
        ((= 1 (length (car vectors))) (error "can't be a matrix!"))
        (else (let ((len (length (car vectors))))
                (accumulate (lambda (cur iter) 
                              (if (not (= (length cur) len))
                                  (error "length mismatch")
                                  (cons cur iter))) 
                            '() vectors)))))

(make-matrix (list 1 2 3) (list 4 5 6) (list 7 8 9))
;(make-matrix)
;(make-matrix 1 2 3 4)
;(make-matrix (list 1) (list 2))



(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (inner op result rest)
    (if (null? rest)
        result
        (inner op (op result (car rest)) (cdr rest))))
  (inner op initial sequence))

;(fold-right / 1 (list 1 2 3))
;(fold-left / 1 (list 1 2 3))
;(fold-right + 0 (list 1 2 3))
;(fold-left + 0 (list 1 2 3))

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(reverse-1 (list 1 2 3 4))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(reverse-2 (list 1 2 3 4))

(define (enumrate-ascending-pair min max)
  (accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j) (list j i))
                        (enumrate-interval 1 (- i 1) 1)))
                   (enumrate-interval min max 1))))

(enumrate-ascending-pair 1 8)
(filter (lambda (x) (prime? (+ (car x) (car (cdr x))))) (enumrate-ascending-pair 1 8))

(define (flatmap func sequence)
  (accumulate append '() (map func sequence)))

(define (remove item sequence)
  (cond ((null? sequence) null)
        ((= item (car sequence)) (cdr sequence))
        (else (cons (car sequence) (remove item (cdr sequence))))))

(define (iter-all-sub-sets sequence)
  (if (null? sequence)
      '()
      (map (lambda (x) (remove x sequence)) sequence)))
  
;(iter-all-sub-sets (list 1 2 3 4))

(define (echo x)
  (print x)
  x)

(define (permutations sequence)
  (define (inner1 x seq)
    (print x)
    (print seq)
    (if (null? seq)
        (list (list x))
        (map (lambda (y) (cons x y)) seq)))
   
  (if (null? sequence)
      '()
      (accumulate append '()
                  (map (lambda (x)
                         (inner1 x (permutations (remove x sequence))))
                       sequence))))

;(permutations (list 1 2 3 4))

(define (safe? positions)
  (define (safe-iter? distance x subseq)
    (cond ((null? subseq) true) 
          ((= x (car subseq)) false)
          ((= (+ x distance) (car subseq)) false)
          ((= (- x distance) (car subseq)) false)
          (else (safe-iter? (+ distance 1) x (cdr subseq)))))
  (safe-iter? 1 (car positions) (cdr positions)))

(define (adjoin-position new-row rest-of-queens)
  (cons new-row rest-of-queens))

(define (gauss-queen-offical k initial-list)
  (if (= k 0)
      (list '())
      (filter
       (lambda (positions) (safe? positions))
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row rest-of-queens))
               initial-list))
        (gauss-queen-offical (- k 1) initial-list)))))

(define (gauss-queen-imp count pos-list)
  (define (check-func? number-list)
    (check-func-imp? 1 (car number-list) (cdr number-list)))
  (define (check-func-imp? depth cur others)
    (cond ((null? others) true)
          ((invalid? cur (car others) depth) false)
          (else (check-func-imp? (+ depth 1) cur (cdr others)))))
  (define (invalid? x y depth)
    (cond ((= x y) true)
          ((= (+ x depth) y) true)
          ((= (- x depth) y) true)
          (else false)))
  (define (expand-pos-sequence seq)
    (accumulate append
                '()
                (map (lambda (x)
                       (map (lambda (y)
                              (cons x y)) seq))
                     pos-list)))
  (if (= 1 count)
      (map (lambda (x) (list x)) pos-list)
      (filter check-func? (expand-pos-sequence (gauss-queen-imp (- count 1) pos-list)))))
  
(define (gauss-queen count)
  (gauss-queen-imp count (enumrate-interval 1 count 1))
  ;(gauss-queen-offical count (enumrate-interval 1 count 1))
  )

(print "gauss-2")
(gauss-queen 2)

(print "gauss-3")
(gauss-queen 3)

(print "gauss-4")
(gauss-queen 4)

(print "gauss-5")
(gauss-queen 5)

(print "gauss-6")
(gauss-queen 6)

(print "gauss-7")
(gauss-queen 7)

(print "gauss-8")
(gauss-queen 8)
