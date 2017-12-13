#lang racket

(define (make-firewall input)
  (define F (make-hash))
  (with-input-from-file input
    (lambda ()
      (for ((ln (in-lines)))
        (define a* (string-split (string-trim ln) ": "))
        (define depth (string->number (car a*)))
        (define range (string->number (cadr a*)))
        (define v (make-vector range '()))
        (vector-set! v 0 '(S up))
        (hash-set! F depth v)
        (void))))
  F)

(define (find-S v)
  (for/first ((i (in-naturals)) (v* (in-vector v)) #:when (memq 'S v*))
    i))

(define (tick! F)
  (for (((k v) (in-hash F)))
    (define i (find-S v))
    (define old (vector-ref v i))
    (vector-set! v i '())
    (define L (vector-length v))
    (define ii
      (if (memq 'up old)
        (if (= (+ i 1) L)
          (- i 1)
          (+ i 1))
        (if (= i 0)
          (+ i 1)
          (- i 1))))
    (define new-val
      (if (memq 'up old)
        (if (= (+ i 1) L)
          '(S down)
          '(S up))
        (if (= i 0)
          '(S up)
          '(S down))))
    (vector-set! v ii new-val)
    (void)))

(define (print-firewall F)
  (printf "FIREWALL~n")
  (for (((k v) (in-hash F)))
    (printf "~a ~a~n" k v)))

(define (go input)
  (define F (make-firewall input))
  (define sev
    (for/sum ((curr-sec (in-range (add1 (max-picos F)))))
      #;(print-firewall F)
      (define penalty
        (if (caught? curr-sec F)
          (get-penalty curr-sec F)
          0))
      (tick! F)
      penalty))
  (define delay
    (let ()
      (define F2 (reduced-firewall F))
      (for/first ((offset (in-naturals))
                  #:when (safe-trip? offset F2))
        offset)
    #;(for/first ((i (in-naturals 11615)) #:when (successful-trip? i input)) i)))
  (printf "part 1, severity : ~a~n" sev)
  (printf "part 2, min delay : ~a~n" delay)
  (void))

(define (reduced-firewall F)
  (for/hash (((k v) (in-hash F)))
    (values k (vector-length v))))

(define (safe-trip? offset RF)
  (for/and ((curr (in-range 0 (+ 1 (max-picos RF)))))
    (or (not (hash-has-key? RF curr))
        (safe-at? (hash-ref RF curr) (+ curr offset)))))

(define (safe-at? N i)
  (not (zero? (modulo i (- (* N 2) 2)))))

(define (successful-trip? i input)
  (define F (make-firewall input))
  (for ((_x (in-range i)))
    (tick! F))
  (for/and ((curr-sec (in-range (add1 (max-picos F)))))
    (and (not (caught? curr-sec F))
         (tick! F)
         #true)))

(define (get-penalty i F)
  (* i (vector-length (hash-ref F i))))

(define (caught? i F)
  (and (hash-has-key? F i)
    (memq 'S (vector-ref (hash-ref F i) 0))))

(define (max-picos F)
  (apply max (hash-keys F)))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-13"
    #:args (INPUT)
    (go INPUT)))
