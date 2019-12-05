#lang racket

(define (part1 input)
  (error 'not-implemented))

(define (part2 input)
  (error 'not-implemented))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-05"
    #:args (INPUT)
    (part1 INPUT)
    (part2 INPUT)
    (void)))
