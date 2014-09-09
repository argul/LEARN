#lang racket

(require "utils.rkt")

(define (count-change amount)
  (cc amount 5))

(define (cc amount kind-of-coins)
  (define result (cond ((= amount 0) 1)
        ((or (< amount 0) (= kind-of-coins 0)) 0)
        (else (+ 
               (cc (- amount 
                      (first-denomination 
                       kind-of-coins))
                     kind-of-coins)
               (cc amount (- kind-of-coins 1))))))
  result)

(define (first-denomination kind-of-coins)
  (cond ((= kind-of-coins 1) 50)
        ((= kind-of-coins 2) 25)
        ((= kind-of-coins 3) 10)
        ((= kind-of-coins 4) 5)
        ((= kind-of-coins 5) 1)))

(count-change 100)
;(cc 2 2)