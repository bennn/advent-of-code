#lang racket

(define secret 368078)

(define (natural->odd level)
  (+ 1 (* level 2)))

(define (coordinates n)
  (for/or ([level (in-naturals 0)])
    (define odd (natural->odd level))
    (and (<= n (* odd odd))
         (cons level (- n (expt (natural->odd (- level 1)) 2))))))

(define (get-distance n)
  (define xq (coordinates n))
  (define x (car xq))
  (define q (cdr xq))
  (+ x (- x (modulo q x))))

(define (neighbors x y)
  (list (list (- x 1) y)
        (list (+ x 1) y)
        (list x (- y 1))
        (list x (+ y 1))
        (list (- x 1) (- y 1))
        (list (+ x 1) (- y 1))
        (list (- x 1) (+ y 1))
        (list (+ x 1) (+ y 1))))

;; Everything is hard-coded .... walk the loop tuple-by-tuple
;;  and build a cache of all previous
(define (get-first-larger k)
  (define H (make-hash '(((0 0) . 1))))
  (define (update! x y)
    (define v
      (for/sum ((ab (neighbors x y)))
        (hash-ref H ab 0)))
    (hash-set! H (list x y) v)
    v)
  (let loop ((x 1) (y 0) (mode 'A))
    (define n (update! x y))
    (if (< k n)
      n
      (case mode
       [(A)
        (if (= x y)
          (loop (- x 1) y 'B)
          (loop x (+ y 1) 'A))]
       [(B)
        (if (= x (- y))
          (loop x (- y 1) 'C)
          (loop (- x 1) y 'B))]
       [(C)
        (if (= x y)
          (loop (+ x 1) y 'D)
          (loop x (- y 1) 'C))]
       [(D)
        (if (= x (- y))
          (loop (+ x 1) y 'A)
          (loop (+ x 1) y 'D))]))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-3"
    #:args ()
    (displayln (get-distance secret))
    (displayln (get-first-larger secret))))
