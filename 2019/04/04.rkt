#lang racket

(define (integer->digit* i)
  (let loop ([acc '()]
             [prev 0]
             [m 1])
    (define m+ (* m 10))
    (define n (quotient (- (modulo i m+) prev) m))
    (define acc+ (cons n acc))
    (if (< i m+)
      acc+
      (loop acc+ (+ prev n) m+))))

(define (parse-range fn)
  (let* ((str (file->string fn))
         (str* (string-split (string-trim str) "-")))
    (apply values (map string->number str*))))

(define (num-digits n)
  (+ 1 (order-of-magnitude n)))

(define (eq-pair n)
  (define str (number->string n))
  (for/or ((i (in-range (- (string-length str) 1))))
    (char=? (string-ref str i) (string-ref str (+ i 1)))))

(define (eq-pair-only n)
  (define group*
    (for/fold ((acc '()))
              ((d (in-list (integer->digit* n))))
      (cond
        [(null? acc)
         (cons (list d) acc)]
        [(= d (caar acc))
         (cons (cons d (car acc)) (cdr acc))]
        [else
         (cons (list d) acc)])))
  (for/or ((g (in-list group*)))
    (= 2 (length g))))

(module+ test
  (require rackunit)
  (test-case "eq-pair-only"
    (check-pred eq-pair-only 112233)
    (check-false (eq-pair-only 123444))
    (check-pred eq-pair-only 111122)))

(define (non-decreasing? n)
  (apply <= (integer->digit* n)))

(define (part1 input)
  (define-values [lo hi] (parse-range input))
  (define num-pwds
    (for/sum ((n (in-range lo hi))
              #:when (= 6 (num-digits n))
              #:when (eq-pair n)
              #:when (non-decreasing? n))
      1))
  (printf "part1 : ~a~n" num-pwds))

(define (part2 input)
  (define-values [lo hi] (parse-range input))
  (define num-pwds
    (for/sum ((n (in-range lo hi))
              #:when (= 6 (num-digits n))
              #:when (eq-pair-only n)
              #:when (non-decreasing? n))
      1))
  (printf "part2 : ~a~n" num-pwds))


(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-04"
    #:args (INPUT)
    (part1 INPUT)
    (part2 INPUT)
    (void)))
