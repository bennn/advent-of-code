#lang racket

(define (solve-captcha filename [part-2? #false])
  (define input
    (let ((s (file->string filename)))
      (substring s 0 (- (string-length s) 1))))
  (define L (string-length input))
  (define L-1 (- L 1))
  (define L/2 (quotient L 2))
  (define (check1 i)
    (if (eq? (string-ref input i) (string-ref input (modulo (+ i 1) L)))
      (string->number (string (string-ref input i)))
      0))
  (define (check2 i)
    (define j (modulo (+ i L/2) L))
    (define a (string-ref input i))
    (define b (string-ref input j))
    (if (eq? a b)
      (string->number (string a))
      0))
  (define checker
    (if part-2? check2 check1))
  (for/sum ((i (in-range L)))
    (checker i)))

(module+ main
  (solve-captcha
    (vector-ref (current-command-line-arguments) 0)
    #true))

