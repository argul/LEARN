#lang racket

;(require racket/stream)
;(stream? ones)
;(stream-empty? ones)
;(stream-first ones)
;(in-stream ones) stream->sequence
;(stream-empty? empty-stream)
;(stream 1) == (stream-cons 1 empty-stream)
;(stream->list stream) => list
;(stream-ref stream 1)
;(stream-tail stream 2) : {1, 2, 3, 4, 5} => {3, 4, 5}

;(define test-stream (stream-cons 1 (stream-cons 2 empty-stream)))
;(stream-length test-stream)

;(define cons-stream stream-cons) // can not be renamed, it's special procedure.
;(define car-stream stream-first)
;(define cdr-stream stream-rest)
;(define stream-null? stream-empty?)

(define (cons-stream first body ...+)
  (cons (first (delay body))))

(define (car-stream stream)
  (car stream))

(define (cdr-stream stream)
  (force (cdr stream)))

(define empty-stream '())
(define stream-null? null?)

;begin infrastructure test block

;end infrastructure test block

(define (stream-1-oper oper stream)
  (stream-cons (oper (car-stream stream))
               (stream-1-oper oper (cdr-stream stream))))

(define (stream-2-oper oper s1 s2)
  (stream-cons (oper (car-stream s1) (car-stream s2))
               (stream-2-oper oper (cdr-stream s1) (cdr-stream s2))))

(define (stream-n-oper oper . streams)
  (stream-cons (oper (map car-stream streams))
               (stream-1-oper oper (map cdr-stream streams))))

(define (stream-head stream n)
  (if (= n 0)
      empty-stream
      (stream-cons (car-stream stream) (stream-head (cdr-stream stream) (- n 1)))))

(define (display-stream stream)
  (begin (stream-for-each (lambda (x) (begin (display x) (display " "))) stream)
         (display "\n")))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (stream-2-oper + ones integers)))

;(stream-ref integers 0)
;(stream-ref (stream-tail integers 2) 0)

;(display-stream (stream-head integers 10))
;(display-stream (stream-head (stream-tail integers 2) 10))