#lang racket

(provide sqrt-threshold)
(provide sin-threshold)
(provide fermat-test-times)
(provide seekfix-tolerate)
(provide devired-dx)

(define sqrt-threshold 0.0001)
(define sin-threshold 0.0001)

(define fermat-test-times 10)
(define seekfix-tolerate 0.0001)
(define devired-dx 0.00001)