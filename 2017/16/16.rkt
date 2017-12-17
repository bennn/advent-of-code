#lang racket

;; TODO this program is correct, but very slow

(define BIL 10 #;00000000)

(define PROG
  '(a b c d e f g h i j k l m n o p))

(define (init)
  (list->vector PROG))

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
  (vector-set! H i b)
  (vector-set! H j a))

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

(define (go input)
  (define H (init))
  (define cmd* (string-split (string-trim (file->string input)) ","))
  (void
    (run! H cmd* 1))
  (define part1 (print-order H))
  (void
    (run! H cmd* (- BIL 1)))
  (define part2 (print-order H))
  (printf "part 1 : ~a~npart 2 : ~a~n" part1 part2)
  (void))

(define (run! H cmd* N)
  (for ((i (in-range N)))
    (for ((cmd (in-list cmd*)))
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
