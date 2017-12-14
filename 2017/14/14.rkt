#lang racket

(require "../10/10.rkt")

(define W 128)
(define H 128)

(define INPUT
  "hxtvlmkl")

(define (make-row-str i)
  (format "~a-~a" INPUT i))

(define (make-grid)
  (for/list ((i (in-range H)))
    (make-row-str i)))

(define (go input)
  (define G (make-grid))
  (define part1
    (for/sum ((r (in-list G)))
      (define o (knot-hash r))
      (for/sum ((c (in-string o)))
        (bits->num-used (hex->bits c)))))
  (define part2
    (count-regions
      (for/vector ((r (in-list G)))
        (define o (knot-hash r))
        (list->vector
          (append* (for/list ((c (in-string o))) (map char->10 (string->list (hex->bits c)))))))))
  (printf "part 1, num used : ~a~n" part1)
  (printf "part 2, num regions : ~a~n" part2)
  (void))

(define (count-regions part2)
  (define num-regions 0)
  (define curr-id "R0")
  (define (incr-id)
    (set! num-regions (+ 1 num-regions))
    (set! curr-id (format "R~a" num-regions))
    (void))
  (define I (vector-length part2))
  (define J (vector-length (vector-ref part2 0)))
  (define (search-from V i j x)
    (unless (= i 0)
      (try-set V (- i 1) j x))
    (unless (= i (- I 1))
      (try-set V (+ i 1) j x))
    (unless (= j 0)
      (try-set V i (- j 1) x))
    (unless (= j (- J 1))
      (try-set V i (+ j 1) x)))
  (define (try-set V i j x)
    (when (equal? (vector-ref** V i j) 1)
      (begin
        (vector-set** V i j x)
        (search-from V i j x))))
  (for* ((i (in-range I)) (j (in-range J)))
    (if (used? (vector-ref** part2 i j))
      (void)
      (if (equal? 1 (vector-ref** part2 i j))
        (begin
          (vector-set** part2 i j curr-id)
          (search-from part2 i j curr-id)
          (incr-id)
          (void))
        (void))))
  #;(for ((v (in-vector part2)))
    (displayln v))
  num-regions)


(define (vector-set** v i j x)
  (vector-set! (vector-ref v i) j x))

(define (used? v)
  (string? v))

(define (vector-ref** v i j)
  (vector-ref (vector-ref v i) j))

(define (char->10 x)
  (if (eq? x #\1) 1 0))

(define (hex->bits c)
  (case c
   ((#\0) "0000")
   ((#\1) "0001")
   ((#\2) "0010")
   ((#\3) "0011")
   ((#\4) "0100")
   ((#\5) "0101")
   ((#\6) "0110")
   ((#\7) "0111")
   ((#\8) "1000")
   ((#\9) "1001")
   ((#\a) "1010")
   ((#\b) "1011")
   ((#\c) "1100")
   ((#\d) "1101")
   ((#\e) "1110")
   ((#\f) "1111")))

(define (bits->num-used b)
  (for/sum ((c (in-string b)))
    (if (eq? c #\1) 1 0)))


(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-14"
    #:args ()
    (go (void))))
