#lang racket

(struct P [p v a] #:transparent)

(define (parse-coordinates str)
  (map string->number (string-split (substring str 1 (sub1 (string-length str))) ",")))

(define (parse-particles input)
  (define H (make-hash))
  (with-input-from-file input
    (lambda ()
      (for ((ln (in-lines)) (i (in-naturals)))
        (define str* (string-split (string-trim ln) ", "))
        (define p (apply P (map parse-coordinates (map (lambda (s) (substring s 2)) str*))))
        (hash-set! H i p))))
    H)

(define (tick-particle p)
  (define pos (P-p p))
  (define vel (P-v p))
  (define acc (P-a p))
  (define vel+
    (map + vel acc))
  (define pos+
    (map + pos vel))
  (P pos+ vel+ acc))

(define (tick! H)
  (for (((k v) (in-hash H)))
    (hash-set! H k (tick-particle v)))
  (void))

(define (find-closest H)
  (for/fold ((dist #f) (part #f) #:result part)
            (((k v) (in-hash H)))
    (define nd (odist v))
    (if (or (eq? #f dist) (< dist nd))
      (values nd k)
      (values dist part))))

(define (odist p)
  (apply + (P-p p)))

(define (run-simulation H)
  (define C (find-closest H))
  (define TIME 100000)
  (let loop ((best C) (timeout TIME))
    (if (zero? timeout)
      best
      (let ()
        (tick! H)
        (define C (find-closest H))
        (if (= best C)
          (loop best (- timeout 1))
          (loop C TIME))))))

(define (find-slowest H)
  (for/fold ((acc #f) (best #f) #:result acc) (((k v) (in-hash H)))
    (if (or (eq? acc #f) (speed< best v))
      (values k v)
      (values acc best))))

(define (speed< p0 p1)
  (define a0 (apply max (map abs (P-a p0))))
  (define a1 (apply max (map abs (P-a p1))))
  (define v0 (apply max (map abs (P-v p0))))
  (define v1 (apply max (map abs (P-v p1))))
  (or (< a0 a1) (and (= a0 a1) (< v0 v1)))
  #;(or (or (< (car a0) (car a1))
          (< (cadr a0) (cadr a1))
          (< (caddr a0) (caddr a1)))
      (and (equal? a0 a1)
           (or (< (car v0) (car v1))
               (< (cadr v0) (cadr v1))
               (< (cadr v0) (cadr v1))))))

(define (go input)
  (define H (parse-particles input))
  (define part1
    #;(run-simulation H)
    (find-slowest H)

    )
  (printf "part 1 : ~a~n" part1)
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-20"
    #:args (INPUT)
    (go INPUT)))
