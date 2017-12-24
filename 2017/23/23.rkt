#lang racket

(define NM (box 0))
(define (NM++) (set-box! NM (add1 (unbox NM))))

(struct program [H q done num-sends] #:transparent)

(define (make-program id)
  (define H (make-hash))
  (program H (box '()) (box '()) (box 0)))

(define (ref H k)
  (hash-ref (program-H H) k 0))

(define (hset! H k v)
  (hash-set! (program-H H) k v))

(define (hupdate! H k v)
  (when (equal? k "h")
    (define old (ref H k))
    (printf "updating h, from ~a to ~a~n" old (v old)))
  (hash-update! (program-H H) k v 0))

(define (interp2 H str)
  (define ss (string-split str))
  (define (eval2 xxx)
    (define n (string->number xxx))
    (if n n (ref H xxx)))
  (match ss
   ((list "set" istr nst)
    (define vvv (eval2 nst))
    (hset! H istr vvv))
   ((list "sub" x y)
    (define vvv (eval2 y))
    (hupdate! H x (lambda (old) (- old vvv))))
   ((list "mul" x y)
    (NM++)
    (define vvv (eval2 y))
    (hupdate! H x (lambda (old) (* old vvv))))
   ((list "jnz" x y)
    (define vvv (eval2 x))
    (define target (eval2 y))
    (when (not (zero? vvv))
      (eval2 y)))
   (_ (error 'badinstr) )))

(define (go input)
  (define V (list->vector (map string-trim (file->lines input))))
  (define LEN (vector-length V))
  (define (do-eval H)
    (define i (box 0))
    (let loop ()
      (define ii (unbox i))
      #;(when (= ii 19)
        (printf "19:~n~a~n" H))
      #;(when (= ii 23)
        (printf "23:~n~a~n" H))
      (when (= ii 27)
        (printf "27:~n~a~n" H))
      (when (= ii 30)
        (printf "30:~n~a~n" H))
      (if (or (< ii 0) (<= LEN ii))
        (string->symbol (format "done ~a~n" ii))
        (let ()
          (define rrr (interp2 H (vector-ref V ii)))
          (cond
           [(eq? rrr 'blocked)
            'blocked]
           [(integer? rrr)
            (define target (+ ii rrr))
            #;(printf "JUMP from ~a to ~a~n" ii target)
            (set-box! i target)
            (loop)]
           [else
            (set-box! i (+ ii 1))
            (loop)])))))
  (define part1
    (let ()
      (define H0 (make-program 0))
      (define rr (do-eval H0))
      (printf "H0 h ~a~n" (ref H0 "h"))
      (unbox NM)))
  (printf "part 1 : ~a~n" part1)
  (define part2
    (let ()
      (define H1 (make-program 1))
      (hset! H1 "a" 1)
      (define rr (do-eval H1))
      (ref H1 "h")))
  (printf "part 2 : ~a~n" part2)
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-23"
    #:args (INPUT)
    (go INPUT)))
