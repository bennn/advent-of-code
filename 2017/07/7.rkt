#lang racket

(define (parse-line ln)
  (define a* (string-split ln "->"))
  (if (= 2 (length a*))
    (let ()
      (define b* (string-split (car a*) " "))
      (define bb (cadr b*))
      (define kids (string-split (string-trim (cadr a*)) ", "))
      (values (car b*) (string->number (substring bb 1 (- (string-length bb) 1))) kids))
    (let ()
      (define b* (string-split (car a*) " "))
      (define bb (cadr b*))
      (values (car b*) (string->number (substring bb 1 (- (string-length bb) 1))) '()))))

(define (program->hash input)
  (define H (make-hash))
  (define (set-parent! kid parent-name)
    (hash-update! H kid
      (lambda (old)
        (list (car old) parent-name (caddr old)))
      (lambda ()
        (list #f parent-name '()))))
  (with-input-from-file input
    (lambda ()
      (for ((ln (in-lines)))
        (define-values [name weight kids] (parse-line ln))
        (hash-update! H name
          (lambda (old)
            (list weight (cadr old) kids))
          (lambda ()
            (list weight #f kids)))
        (for ((k (in-list kids)))
          (set-parent! k name))
        (void))))
  #;(for (((k v) (in-hash H)))
    (printf "  ~a ~a~n" k v))
  H)

(define (go input [day1? #true])
  (define h (program->hash input))
  (define start
    (for/first (((k v) (in-hash h)) #:when (eq? #f (cadr v)))
      k))
  (if day1?
    start
    (let ()
  (define (get-weight k)
    (let loop ([k k])
      (define v (hash-ref h k))
      (+ (car v)
         (apply + (map loop (caddr v))))))
  (define (solve name kw)
    (define kids (caddr (hash-ref h name)))
    (define w* (map get-weight kids))
    (define-values [ok bad _idx] (sort-out w*))
    (- ok kw))
  (let loop ((curr start))
    (define v (hash-ref h curr))
    (define-values [weight parent kids] (values (car v) (cadr v) (caddr v)))
    (define w* (map get-weight kids))
    (if (apply = w*)
      (begin
        (solve parent (apply + w*)))
      (let ()
        (define-values [_a _b idx] (sort-out w*))
        (loop (list-ref kids idx))))))))

(define (sort-out n*)
  (let loop ([i 0])
    (define q (list-ref n* i))
    (define num
      (for/sum ([b (in-list n*)])
        (if (= q b) 1 0)))
    (if (= num 1)
      (values (for/first ((b (in-list n*)) #:when (not (= q b))) b) q i)
      (loop (+ i 1)))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-7"
    #:args (INPUT)
    (go INPUT #false)))
