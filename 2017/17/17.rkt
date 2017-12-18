#lang racket

(define INPUT 349)

(define (fast-buffer step-size N)
  (define position 0)
  (define result 0)
  (for ((size (in-range 1 (+ 1 N))))
    (set! position (+ (modulo (+ position step-size) size) 1))
    (when (= position 1)
      #;(printf "new result ~a~n" size)
      (set! result size)))
  result)

(define (simulate-buffer step-size N)
  (define b+
    (for/fold ((b (init-buffer)) (prev #f) #:result b)
              ((i (in-range 1 (+ N 1))))
      (define bs (bitwise-bit-set? i 18))
      #;(when (not (eq? prev bs))
        (displayln i))
      (values (minsert (march b step-size) i) bs)))
  #;(displayln b+)
  b+)

(define (march b n)
  (if (zero? n)
    b
    (march (mcdr b) (- n 1))))

(define (minsert b v)
  (define cell (mcons v #f))
  (define tl (mcdr b))
  (set-mcdr! b cell)
  (set-mcdr! cell tl)
  cell)

(define (init-buffer)
  (define b0 (mcons 0 #f))
  (set-mcdr! b0 b0)
  b0)

(define (mindex-of b v)
  (let loop ((i 0) (b b))
    (if (= v (mcar b))
      i
      (loop (+ i 1) (mcdr b)))))

(define (mlist-ref b i)
  (if (= i 0)
    (mcar b)
    (mlist-ref (mcdr b) (- i 1))))

(define (go)
  (define part1
    (let ((b (simulate-buffer INPUT 2017)))
      (mlist-ref b (+ 1 (mindex-of b 2017)))))
  (define part2
    (fast-buffer INPUT (* 50 (expt 10 6))))
  (printf "part 1 : ~a~npart 2 : ~a~n" part1 part2)
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-17"
    #:args ()
    (go)))
