#lang racket

(define R
  (make-hash))

(define (largest)
  (for/fold ([acc #f])
            (((k v) (in-hash R)))
    (if (or (not acc) (> v acc)) v acc)))

(define (ref var)
  (hash-ref! R var 0))

(define highest (box 0))

(define (set var val)
  (hash-update! R var
    (lambda (old)
      (define new (+ old val))
      (when (> new (unbox highest)) (set-box! highest new))
      new)
    (lambda () 0)))

(define (go input)
  (with-input-from-file input
    (lambda ()
      (for ((ln (in-lines)))
        (do-instruction (parse-line ln)))))
  (largest)
  (unbox highest))

(define (printall)
  (for (((k v) (in-hash R)))
    (printf " ~a : ~a~n" k v)))

(define (parse-line ln)
  (define a* (string-split (string-trim ln) " if "))
  (define cond (cadr a*))
  (define b* (string-split (car a*)))
  (list (car b*) (parse-op (cadr b*)) (string->number (caddr b*)) (parse-cond cond)))

(define (parse-op str)
  (cond
   [(string=? str "dec")
    -]
   [(string=? str "inc")
    +]
   [else
    (error 'bad "bad op ~a~n" str)]))

(define (parse-cond cond)
  (define a* (string-split cond))
  (define v1 (string->number (caddr a*)))
  (define op (eval-op (cadr a*)))
  (lambda ()
    (define v0 (ref (car a*)))
    (op v0 v1)))

(define (eval-op str)
  (cond
   [(equal? str "!=")
    (lambda (a b)
      (not (= a b)))]
   [(equal? str ">")
    >]
   [(equal? str "<")
    <]
   [(equal? str ">=")
    >=]
   [(equal? str "<=")
    <=]
   [(equal? str "==")
    =]
   [else
   (error 'asdf "bad op ~a~n" str)]))

(define (do-instruction i)
  (define r (car i))
  (define op (cadr i))
  (define val (caddr i))
  (define cond (cadddr i))
  (when (cond)
    (set r (op val))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-8"
    #:args (INPUT)
    (go INPUT)))
