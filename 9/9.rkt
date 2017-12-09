#lang racket

(define (file->vector f)
  (with-input-from-file f
    (lambda ()
      (for/vector ((ln (in-lines)))
        (string->number (string-trim ln))))))

(define (go input)
  (with-input-from-file input
    (lambda ()
      (define f (box 0))
      (define garbage-mode? (box #f))
      (define score (box 0))
      (define g (box 1))
      (let loop ()
        (define c (read-char))
        (cond
         [(eof-object? c)
          (void)]
         [(unbox garbage-mode?)
          (cond
           [(eq? c #\>)
            (set-box! garbage-mode? #false)]
           [(eq? c #\!)
            (read-char)]
           [else
            (set-box! f (+ (unbox f) 1))
            (void)])
          (loop)]
         [(and (not (unbox garbage-mode?)) (eq? c #\<))
          (set-box! garbage-mode? #true)
          (loop)]
         [(eq? c #\{)
          ;(printf "score g ~a ~a~n" (unbox score) (unbox g))
          (set-box! score (+ (unbox score) (unbox g)))
          (set-box! g (+ (unbox g) 1))
          (loop)]
         [(eq? c #\})
          ;(printf "2222 score g ~a ~a~n" (unbox score) (unbox g))
          (set-box! g (- (unbox g) 1))
          (loop)]
         [else
          (loop)]))
      (unbox f))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-9"
    #:args (INPUT)
    (go INPUT)))
