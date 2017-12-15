#lang racket

(define fA 16807)
(define fB 48271)
(define div 2147483647)

(define NUM (* 5 1000000))

(define (A-next prev)
  (let loop ((n (gen-next prev fA)))
    (if (zero? (modulo n 4))
      n
      (loop (gen-next n fA)))))

(define (B-next prev)
  (let loop ((n (gen-next prev fB)))
    (if (zero? (modulo n 8))
      n
      (loop (gen-next n fB)))))

(define (gen-next prev x)
  (remainder (* prev x) div))

(define (nat->bin n)
  (~r n #:base 2 #:min-width 32 #:pad-string "0"))

(define (cmp n0 n1)
  (=
    (subs n0) (subs n1)))

(define (subs n)
  (string->number (substring (nat->bin n) (- 32 16)) 2))

(define (mk input)
  (define H (make-hash))
  (with-input-from-file input
    (lambda ()
      (for ((ln (in-lines)))
        (define lnn (string-trim ln))
        (void))))
  H)

(define (go)
  (define a 618)
  (define b 814)
  (for/sum ((round (in-range NUM)))
    #;(printf "r ~a~n" round)
    (begin0 (if (cmp a b) 1 0)
      (set! a (A-next a))
      (set! b (B-next b)))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-15"
    #:args ()
    (go)))
