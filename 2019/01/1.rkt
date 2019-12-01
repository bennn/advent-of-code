#lang racket/base

(define (fuel-req* fn [day2 #f])
  (define gf (if day2 get-fuel* get-fuel))
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

(define (day1 fn)
  (printf "day1 : ~a~n" (apply + (fuel-req* fn))))

(define (day2 fn)
  (printf "day2 : ~a~n" (apply + (fuel-req* fn #true))))

(module+ main
         (require racket/cmdline)
         (command-line
           #:args (in)
           (day1 in)
           (day2 in)
           (void)))
