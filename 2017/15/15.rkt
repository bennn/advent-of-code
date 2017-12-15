#lang racket
(require racket/performance-hint)
(begin-encourage-inline
(define fA 16807)
(define fB 48271)
(define div 2147483647)

(define MIL 1000000)
(define NUM-1 (* 40 MIL))
(define NUM-2 (* 5 MIL))

(define (A-next-1 prev)
  (gen-next prev fA))

(define (B-next-1 prev)
  (gen-next prev fB))

(define (A-next-2 prev)
  (let loop ((prev prev))
    (define n (gen-next prev fA))
    (if (div4? n)
      n
      (loop n))))

(define (B-next-2 prev)
  (let loop ((prev prev))
    (define n (gen-next prev fB))
    (if (div8? n)
      n
      (loop n))))

(define (div4? n)
  (zero? (bitwise-and n 3)))

(define (div8? n)
  (zero? (bitwise-and n 7)))

(define (gen-next prev x)
  (remainder (* prev x) div))

(define (cmp n0 n1)
  (=
    (subs n0) (subs n1)))

(define subs
  (let ([sixteen-ones (- (expt 2 16) 1)])
    (lambda (n) (bitwise-and n sixteen-ones))))

(define (go)
  (define a0 618)
  (define b0 814)
  (define part1
    (let ((a a0) (b b0))
      (for/sum ((round (in-range NUM-1)))
        (begin0 (if (cmp a b) 1 0)
          (set! a (A-next-1 a))
          (set! b (B-next-1 b))))))
  (define part2
    (let ((a a0) (b b0))
      (for/sum ((round (in-range NUM-2)))
        (begin0 (if (cmp a b) 1 0)
          (set! a (A-next-2 a))
          (set! b (B-next-2 b))))))
  (printf "part1 : ~a~n" part1)
  (printf "part2 : ~a~n" part2)
  (void))
)

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-15"
    #:args ()
    (go)))

