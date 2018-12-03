#lang racket

(define (go input)
  (apply + (map string->number (file->lines input))))

(define (go2 input)
  (define total (box 0))
  (define seen (make-hash (list (cons (unbox total) #f))))
  (for/or ((n (in-cycle (in-list (map string->number (file->lines input))))))
    (set-box! total (+ (unbox total) n))
    (if (hash-has-key? seen (unbox total))
      (unbox total)
      (begin (hash-set! seen (unbox total) #f) #f))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-01"
    #:args (INPUT)
    (go2 INPUT)))
