#lang racket

(define TAIL '(17 31 73 47 23))

(define (string->ascii str)
  (for/list ((c (in-string str)))
    (char->integer c)))

(define (knot-hash str)
  (define lengths (append (string->ascii str) TAIL))
  (define MAGIC 256)
  (define NUM-ROUNDS 64)
  (define nums (build-vector MAGIC values))
  (define (reverse! curr ln)
    (define sub (for/list ((i (in-range ln))) (vector-ref nums (modulo (+ curr i) MAGIC))))
    (for ((i (in-range ln))
          (k (in-list (reverse sub))))
      (vector-set! nums (modulo (+ curr i) MAGIC) k)))
  (define curr 0)
  (define skip 0)
  (for ((_round-num (in-range NUM-ROUNDS)))
    (for ((ln (in-list lengths)))
      (reverse! curr ln)
      (set! curr (modulo (+ curr (+ ln skip)) MAGIC))
      (set! skip (modulo (+ skip 1) MAGIC))
      (void))
    (void))
  (hex->hash (do-xor nums)))

(define (hex->hash n*)
  (string-join (for/list ((i (in-list n*)))
    (define s (~r i #:base 16))
    (if (= 1 (string-length s)) (string-append "0" s) s)) ""))

(define (do-xor nums)
  (let loop ((i 0))
    (if (= i 256)
      '()
      (cons (xor* (for/list ([kk (in-range i (+ i 16))]) (vector-ref nums kk)))
        (loop (+ i 16))))))

(define (xor* num*)
  (apply bitwise-xor num*))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-10"
    #:args (INPUT)
    (knot-hash INPUT)))
