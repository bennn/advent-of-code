#lang racket

(define (maker input)
  (with-input-from-file input
    (lambda ()
      (for/vector ((ln (in-lines)))
        (string->number ln)))))

(define (go input [day2? #false])
  (define v (maker input))
  (define L (vector-length v))
  (let loop ((i 0) (steps 0))
    (if (or (< i 0) (<= L i))
      steps
      (let ()
        (define offset (vector-ref v i))
        (if (and day2? (<= 3 offset))
          (vector-set! v i (- offset 1))
          (vector-set! v i (+ offset 1)))
        (loop (+ i offset)
              (+ steps 1))))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-5"
    #:args (INPUT)
    (go INPUT)))
