#lang racket

(define (file->map input)
  (with-input-from-file input
    (lambda ()
      (for/vector ((ln (in-lines)))
        (for/vector ((c (in-string (substring ln 0 (- (string-length ln) 1)))))
          c)))))

(define ALPHA (for/list ((c (in-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    c))

(define (pos++ pos)
  (define a (car pos))
  (define b (cdr pos))
  (list (cons a (+ b 1))
        (cons a (- b 1))
        (cons (+ a 1) b)
        (cons (- a 1) b)))

(define (go input)
  (define M (file->map input))
  (define old '())
  (define NUM-ROWS (vector-length M))
  (define NUM-COLS (vector-length (vector-ref M 0)))
  (define (vector-ref2 pos)
    (define a (car pos))
    (define b (cdr pos))
    (and (<= 0 a (- NUM-COLS 1))
         (<= 0 b (- NUM-ROWS 1))
         (vector-ref (vector-ref M b) a)))
  (define (change-dir pos prev)
    (for/first ((other (in-list (pos++ pos)))
                (new-dir (in-list '(S N E W)))
                #:when (and (not (equal? other prev))
                            (let ((cc (vector-ref2 other)))
                              (and cc (not (eq? #\space cc))))))
      (list other new-dir)))
  (define num-steps 0)
  (define (num-steps++)
    (set! num-steps (+ num-steps 1)))
  (define (save-pos! c pos)
    (set! old (cons (cons c pos) old)))
  (define (init-pos!)
    (let loop ((pos (cons 0 0)))
      (if (eq? (vector-ref2 pos) #\|)
        pos
        (loop (+D pos 'E)))))
  (let loop ((pos (init-pos!)) (dir 'S))
    (num-steps++)
    (define c (vector-ref2 pos))
    (cond
     [(eq? #f c)
      (void)]
     [(or (eq? c #\|) (eq? c #\-))
      (loop (+D pos dir) dir)]
     [(or (eq? c #\+) (memq c ALPHA))
      (when (memq c ALPHA)
        (save-pos! c pos))
      (define nxt (+D pos dir))
      (define c2 (vector-ref2 nxt))
      (if (or (eq? #f c2) (eq? #\space c2))
        (let ()
          (define new-stuff (change-dir pos (-D pos dir)))
          (if new-stuff
            (loop (car new-stuff) (cadr new-stuff))
            (void)))
        (loop nxt dir))]
     [else
      (error 'badchar "~a" c)]))
  (define part1
    (apply string (map car (reverse old))))
  (printf "part 1 : ~a~n" part1)
  (printf "part 2 : ~a~n" num-steps)
  (void))

(define (-D pos dir)
  (case dir
   ((N)
    (cons (car pos) (+ 1 (cdr pos))))
   ((S)
    (cons (car pos) (- (cdr pos) 1)))
   ((E)
    (cons (- (car pos) 1) (cdr pos)))
   ((W)
    (cons (+ (car pos) 1) (cdr pos)))
   (else
    (error 'baddir "~a" dir))))

(define (+D pos dir)
  (case dir
   ((N)
    (cons (car pos) (- (cdr pos) 1)))
   ((S)
    (cons (car pos) (+ (cdr pos) 1)))
   ((E)
    (cons (+ 1 (car pos)) (cdr pos)))
   ((W)
    (cons (- (car pos) 1) (cdr pos)))
   (else
    (error 'baddir "~a" dir))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-19"
    #:args (INPUT)
    (go INPUT)))
