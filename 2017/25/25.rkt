#lang racket


(define (go input)
  (define TAPE
    (make-hash))
  (define CURSOR
    0)
  (define (move-right!)
    (set! CURSOR (+ 1 CURSOR)))
  (define (move-left!)
    (set! CURSOR (- CURSOR 1)))
  (define (ref k)
    (hash-ref TAPE k 0))
  (define (tape-set! k v)
    (hash-set! TAPE k v))
  (define STATE
    'A)

  (define STEPS
    12656374)
  (define (get-checksum)
    (for/sum ((v (in-hash-values TAPE)))
      v))

  (let loop ((step 0))
    (if (= step STEPS)
      (void)
      (let ()
        (define CV (ref CURSOR))
        (case STATE
         ((A)
          (cond
           [(zero? CV)
            (tape-set! CURSOR 1)
            (move-right!)
            (set! STATE 'B)
            (void)]
           [(= 1 CV)
            (tape-set! CURSOR 0)
            (move-left!)
            (set! STATE 'C)
            (void)]
           [else
            (error 'A)]))
         ((B)
          (cond
           [(zero? CV)
            (tape-set! CURSOR 1)
            (move-left!)
            (set! STATE 'A)
            (void)]
           [(= 1 CV)
            (tape-set! CURSOR 1)
            (move-left!)
            (set! STATE 'D)
            (void)]
           [else
            (error 'B)]))
         ((C)
          (cond
           [(= CV 0)
            (tape-set! CURSOR 1)
            (move-right!)
            (set! STATE 'D)
            (void)]
           [(= CV 1)
            (tape-set! CURSOR 0)
            (move-right!)
            (set! STATE 'C)
            (void)]
           [else
            (error 'C)]))
         ((D)
          (cond
           [(= CV 0)
            (tape-set! CURSOR 0)
            (move-left!)
            (set! STATE 'B)
            (void)]
           [(= CV 1)
            (tape-set! CURSOR 0)
            (move-right!)
            (set! STATE 'E)
            (void)]
           [else
            (error 'D)]))
         ((E)
          (cond
           [(= CV 0)
            (tape-set! CURSOR 1)
            (move-right!)
            (set! STATE 'C)
            (void)]
           [(= CV 1)
            (tape-set! CURSOR 1)
            (move-left!)
            (set! STATE 'F)
            (void)]
           [else
            (error 'E)]))
         ((F)
          (cond
           [(= CV 0)
            (tape-set! CURSOR 1)
            (move-left!)
            (set! STATE 'E)
            (void)]
           [(= CV 1)
            (tape-set! CURSOR 1)
            (move-right!)
            (set! STATE 'A)
            (void)]
           [else
            (error 'F)]))
         (else (error 'badstate "~a" STATE)))
    (loop (+ 1 step)))
  ))
  (define part1
    (get-checksum))
  part1
)

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-25"
    #:args (INPUT)
    (go INPUT)))
