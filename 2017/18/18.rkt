#lang racket

(struct program [H q done num-sends] #:transparent)

(define (make-program id)
  (define H (make-hash))
  (hash-set! H "p" id)
  (program H (box '()) (box '()) (box 0)))

(define H0 (make-program 0))
(define H1 (make-program 1))

;(define part1 (box #f))
;(define LAST-SOUND (box #f))
;(define (play-sound n)
;  #;(printf "play ~a~n" n)
;  (set-box! LAST-SOUND n))
;(define last-sound
;  (let ((num-calls (box 0)))
;    (lambda ()
;      #;(printf "rcv~n")
;      (when (equal? (unbox num-calls) 0)
;        (set-box! part1 (unbox LAST-SOUND))
;        #;(printf "answer ~a~n" (unbox part1))
;        (void))
;      (set-box! num-calls (+ (unbox num-calls) 1))
;      (unbox LAST-SOUND))))

(define (play-sound H n)
  ;(define q (program-q H))
  ;(define snd (car (unbox q)))
  ;(set-box! q (cdr (unbox q)))
  (define done (program-done H))
  (set-box! done (cons n (unbox done)))
  (void))

(define (last-sound H)
  (define zzz (unbox (program-done H)))
  (if (null? zzz)
    (printf "ERROR no last~n")
    (begin
      (printf "last sound ~a~n" (car zzz))
      (error 'done)
      )))

(define (interp H str)
  (define ss (string-split str))
  (define (eval2 xxx)
    (define n (string->number xxx))
    (if n n (ref H xxx)))
  (match ss
   ((list "set" istr nst)
    (hset! H istr (eval2 nst)))
   ((list "snd" letter)
    (play-sound H (eval2 letter)))
   ((list "add" x y)
    (hupdate! H x (lambda (old) (+ old (eval2 y)))))
   ((list "mul" x y)
    (hupdate! H x (lambda (old) (* old (eval2 y)))))
   ((list "mod" x y)
    (hupdate! H x (lambda (old) (modulo old (eval2 y)))))
   ((list "rcv" x)
    (unless (zero? (eval2 x))
      (last-sound H)))
   ((list "jgz" x y)
    (when (> (eval2 x) 0)
      (eval2 y)))
   (_ (void))))

(define (send-val dst v)
  (define-values [H me] (if (= dst 0) (values H0 H1) (values H1 H0)))
  (define rcv (program-q H))
  (set-box! rcv (cons v (unbox rcv)))
  (define ns (program-num-sends me))
  (set-box! ns (+ (unbox ns) 1))
  (void))

(define (recv-val dst)
  ;(semaphore-wait LOCK)
  (define-values [H me] (if (= dst 0) (values H0 H1) (values H1 H0)))
  (define q (program-q me))
  (define v (unbox q))
  (if (null? v)
    'blocked
    (begin0 (last v) (set-box! q (drop-right v 1)))))

(define (interp2 H dst str)
  (define ss (string-split str))
  (define (eval2 xxx)
    (define n (string->number xxx))
    (if n n (ref H xxx)))
  (match ss
   ((list "set" istr nst)
    (define vvv (eval2 nst))
    (hset! H istr vvv))
   ((list "snd" letter)
    (define vvv (eval2 letter))
    (send-val dst vvv))
   ((list "add" x y)
    (define vvv (eval2 y))
    (hupdate! H x (lambda (old) (+ old vvv))))
   ((list "mul" x y)
    (define vvv (eval2 y))
    (hupdate! H x (lambda (old) (* old vvv))))
   ((list "mod" x y)
    #;(printf "mod ~a ~a ~a~n" x y H)
    (define vvv (eval2 y))
    (hupdate! H x (lambda (old) (modulo old vvv))))
   ((list "rcv" x)
    (define rrr (recv-val dst))
    (if (eq? 'blocked rrr)
      'blocked
      (hset! H x rrr)))
   ((list "jgz" x y)
    (define vvv (eval2 x))
    (when (> vvv 0)
      (eval2 y)))
   (_ (error 'badinstr) )))

(define (ref H k)
  (hash-ref (program-H H) k 0))

(define (hset! H k v)
  (hash-set! (program-H H) k v))

(define (hupdate! H k v)
  (hash-update! (program-H H) k v 0))

(define (go input)
  (define V (list->vector (map string-trim (file->lines input))))
  (define LEN (vector-length V))
  (define (make-do-eval H)
    (define dst (modulo (+ 1 (hash-ref (program-H H) "p")) 2))
    (define i (box 0))
    (lambda ()
      (if (or (< (unbox i) 0) (<= LEN (unbox i)))
        (string->symbol (format "done ~a~n" (unbox i)))
        (let ()
          (define rrr (interp2 H dst (vector-ref V (unbox i))))
          (cond
           [(eq? rrr 'blocked)
            'blocked]
           [(integer? rrr)
            (set-box! i (+ (unbox i) rrr))
            'ok]
           [else
            (set-box! i (+ (unbox i) 1))
            'ok])))))
  (define P0 'ok)
  (define P1 'ok)
  (define F0 (make-do-eval H0))
  (define F1 (make-do-eval H1))
  (let loop ()
    (cond
     [(eq? 'ok P0)
      (set! P0 (F0))
      (loop)]
     [(eq? 'ok P1)
      (set! P1 (F1))
      (loop)]
     [(and (eq? 'blocked P1) (has-messages? H1))
      (set! P1 'ok)
      (loop)]
     [(and (eq? 'blocked P0) (has-messages? H0))
      (set! P0 'ok)
      (loop)]
     [else
      (void)]))
  (printf "send 0 : ~a~n" (unbox (program-num-sends H0)))
  (printf "send 1 : ~a~n" (unbox (program-num-sends H1)))
  (void))

(define (has-messages? H)
  (not (null? (unbox (program-q H)))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-18"
    #:args (INPUT)
    (go INPUT)))
