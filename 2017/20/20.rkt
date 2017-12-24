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

(define (collide! H)
  (define C (make-hash))
  (for (((k v) (in-hash H)))
    (hash-update! C (P-p v) (lambda (old) (cons k old)) '()))
  (for* ((k* (in-hash-values C))
         (k (in-list k*)))
    (hash-remove! H k))
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

(define (run-simulation! H)
  (define TIME 100000)
  (let loop ((timeout TIME))
    (if (zero? timeout)
      (void)
      (let ()
        (tick! H)
        (if (collide! H)
          (loop TIME)
          (loop (- timeout 1)))))))

(define (find-slowest H)
  (define zero-acc
    (for/list (((k v) (in-hash H)) #:when (zero-vector? (P-a v)))
      (cons k v)))
  (for/fold ((acc #f) (best #f) #:result acc)
            ((kv (in-list zero-acc)))
    (define-values [k v] (values (car kv) (cdr kv)))
    (if (or (eq? #f acc) (vel< v best))
      (values k v)
      (values acc best))))

(define (zero-vector? xxx)
  (andmap zero? xxx))

(define (acc< p0 p1)
  (particle< P-a p0 p1))

(define (vel< p0 p1)
  (particle< P-v p0 p1))

(define (particle< accessor p0 p1)
  (define a0 (apply + (map abs (accessor p0))))
  (define a1 (apply + (map abs (accessor p1))))
  (< a0 a1))

(define (get-untouched H)
  (define hitmap
    (particles->collides-when H))
  (define pairs
    (match-up hitmap))
  (for/list ((k (in-hash-keys H))
             #:when (unmatched? k pairs))
    k))

(define (particles->collides-when H)
  TODO)

(define (match-up hitmap)
  TODO)

(define (unmatched? k pairs)
  (if (null? pairs)
    #true
    (and (not (memv k (car pairs))) (unmatched? k (cdr pairs)))))

(define (go input)
  (define part1
    (let ()
      (define H (parse-particles input))
      (find-slowest H)))
  (define part2
    (let ()
      (define H (parse-particles input))
      (length (get-untouched H))))
  (printf "part 1 : ~a~n" part1)
  (printf "part 2 : ~a~n" part2)
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-20"
    #:args (INPUT)
    (go INPUT)))
