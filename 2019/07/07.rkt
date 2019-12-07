#lang racket

(define (part1 input)
  (error 'not-implemented))

(define (part2 input)
  (error 'not-implemented))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-07"
    #:args (INPUT)
    (printf "part1: ~a~n" (part1 INPUT))
    (printf "part2: ~a~n" (part2 INPUT))
    (void)))
