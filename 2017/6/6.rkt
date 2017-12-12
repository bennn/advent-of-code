#lang racket

(define V (vector
  0	5	10	0	11	14	13	4	11	8	8	7	1	4	12	11))
(define L (vector-length V))

(define seen
  (make-hash))

(define (vector->data V)
  (for/list ((i (in-vector V))) i))


(define (go input)
  (let loop ((steps 0))
    (define d (vector->data V))
    (if (hash-has-key? seen d)
      (- steps (hash-ref seen d))
      (let ()
        (hash-set! seen d steps)
        (define i (find-max V))
        (define ii (vector-ref V i))
        (reallocate! V ii i)
        (loop (+ steps 1))))))

(define (find-max V)
  (car (for/fold ([acc (cons #f #f)])
            ([i (in-range (vector-length V))])
    (define prev (cdr acc))
    (define curr (vector-ref V i))
    (if (or (eq? #f prev)
            (> curr prev))
      (cons i curr)
      acc))))

(define (reallocate! V val start-index)
  (vector-set! V start-index 0)
  (let loop ([val val]
             [i (modulo (+ start-index 1) L)])
    (if (zero? val)
      (void)
      (let ()
        (vector-set! V i (+ (vector-ref V i) 1))
        (loop (- val 1) (modulo (+ i 1) L))))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-6"
    #:args (INPUT)
    (go INPUT)))
