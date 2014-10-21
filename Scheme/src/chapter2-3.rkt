#lang racket

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

(define test (list (list 1 2) (list 3 4)))

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

(subset (list 1 2 3 4))