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
    (get-hitmap H))
  (printf "HM ~a~n" hitmap)
  (define xxx
    (match-up hitmap))
  (printf "matched ~a~n" xxx)
  (for/list ((k (in-hash-keys H))
             #:when (unmatched? k xxx))
    k))

(define (get-hitmap H)
  (let loop ((p* (hash->list H)) (HM (hash)))
    (if (null? p*)
      HM
      (let ()
        (define-values [k0 v0] (values (car (car p*)) (cdr (car p*))))
        (define p-rest (cdr p*))
        (define V (get-one-hitmap v0 p-rest))
        (loop p-rest (hitmap-set* HM k0 V))))))

(define (hitmap-set* HM k0 tp*)
  (for/fold ((acc HM))
            ((tp (in-list tp*)))
    (define t (car tp))
    (define k1 (cdr tp))
    (define newp (make-pair k0 k1))
    (hash-update HM t (lambda (old) (cons newp old)) '())))

(define (make-pair n0 n1)
  (if (< n0 n1)
    (cons n0 n1)
    (cons n1 n0)))

(define (get-one-hitmap p0 kv*)
  (filter values
    (for/list ((kv (in-list kv*)))
      (define k (car kv))
      (define v (cdr kv))
      (define t (collides? p0 v))
      (and t (cons t k)))))

(define NUM (expt 2 16))

(define (collides? p0 p1)
  (define e0 (particle->equation p0))
  (define e1 (particle->equation p1))
  (let loop ([t 0] [prev #f])
    #;(when (modulo t NUM)
      (printf "... checking collides? ~a ~a~n" p0 p1))
    (define pos0 (e0 t))
    (define pos1 (e1 t))
    (define dist (distance pos0 pos1))
    (cond
     [(= 0 dist)
      t]
     [(or (eq? prev #f) (>= prev dist))
      (loop (+ t 1) dist)]
     [else
      #false])))

(define (make-get-position vx ax)
  (lambda (t)
    (+ (* vx t) (* 1/2 ax (expt t 2)))))

(define (particle->equation p)
  (define init-pos (P-p p))
  (define-values [get-x get-y get-z]
    (apply values (map make-get-position (P-v p) (P-a p))))
  (lambda (t)
    (map + (list (get-x t) (get-y t) (get-z t)) init-pos)))

(define (distance pos0 pos1)
  (sqrt (apply + (map (lambda (x0 x1) (expt (- x1 x0) 2)) pos0 pos1))))

(define (match-up HM)
  (let loop ((t* (sort (hash-keys HM) <)) (HM HM))
    (if (null? t*)
      '()
      (let ()
        (define t (car t*))
        (define v* (hash-ref HM t))
        (append v* (loop (cdr t*) (hitmap-remove* HM t v*)))))))

(define (hitmap-remove* HM t v*)
  (for/fold ((acc HM))
            (((k v) (in-hash HM))
             #:when (< t k))
    (hash-set HM k (remove* v* v))))

(define (unmatched? k xxx)
  (not (memv k xxx)))

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
