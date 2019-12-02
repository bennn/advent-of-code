#lang racket/base

(define (fuel-req* fn [part2 #f])
  (define gf (if part2 get-fuel* get-fuel))
  (with-input-from-file
    fn
    (lambda ()
      (for/list ((ln (in-lines)))
        (gf (string->number ln))))))

(define (get-fuel n)
  (- (quotient n 3) 2))

(define (get-fuel* n)
  (define f (get-fuel n))
  (if (< f 0)
    0
    (+ f (get-fuel* f))))

(define (part1 fn)
  (printf "part1 : ~a~n" (apply + (fuel-req* fn))))

(define (part2 fn)
  (printf "part2 : ~a~n" (apply + (fuel-req* fn #true))))

(module+ main
         (require racket/cmdline)
         (command-line
           #:args (in)
           (part1 in)
           (part2 in)
           (void)))
