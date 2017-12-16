#lang racket

(define PROG
  '(a b c d e f g h i j k l m n o p))

(define (init)
  (list->vector PROG))
;  (for/hash ((k (in-list)) (v (in-naturals)))
;    (values k v)))

(define (do-spin H x)
  ;; x programs from end to front
  (define v* (vector->list H))
  (define-values [pre post] (split-at-right v* x))
  (for ((v (in-list (append post pre))) (i (in-naturals)))
    (vector-set! H i v))
  (void))

(define (do-exchange H a b)
  (define p0 (vector-ref H a))
  (define p1 (vector-ref H b))
  (vector-set! H a p1)
  (vector-set! H b p0)
  (void))

(define (do-partner H a b)
  (define i (findi H a))
  (define j (findi H b))
  (do-exchange H i j))

(define (spin? cmd)
  (and (< 0 (string-length cmd)) (equal? #\s (string-ref cmd 0))))

(define (exchange? cmd)
  (and (< 0 (string-length cmd)) (equal? #\x (string-ref cmd 0))))

(define (partner? cmd)
  (and (< 0 (string-length cmd)) (equal? #\p (string-ref cmd 0))))

(define (parse-spin cmd)
  (list (string->number (substring cmd 1))))

(define (parse-exchange cmd)
  (map string->number (string-split (substring cmd 1) "/")))

(define (parse-partner cmd)
  (map string->symbol (string-split (substring cmd 1) "/")))

(define (print-order H)
  (string-join (map symbol->string (vector->list H)) "") #;
  (values ;string-join
    (for/list ((p (in-list PROG)))
      (number->string (findi H p))) ""))

(define (findi H p)
  (for/first ((i (in-naturals)) (v (in-vector H)) #:when (eq? v p))
    i))

(define BIL 1000000000)

(define (go input)
  (define H (init))
  (define str (string-trim (file->string input)))
  (for ((i (in-range BIL)))
    (for ((cmd (in-list (string-split str ","))))
      (cond
       [(spin? cmd)
        (apply do-spin H (parse-spin cmd))]
       [(exchange? cmd)
        (apply do-exchange H (parse-exchange cmd))]
       [(partner? cmd)
        (apply do-partner H (parse-partner cmd))]
       [else
        (error 'badcommand "~a" cmd)])))
   (print-order H))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-16"
    #:args (INPUT)
    (go INPUT)))
