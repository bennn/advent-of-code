#lang racket

(define w 25)
(define h 6)

(define (parse in)
  )

(define (file->layer* img)
  )

(define (count-bits layer n)
  )

(define (part1 input)
  (define layer* (file->layer* input))
  (for/fold ((acc #f)
             (zeros #f)
             #:result (* (count-bits acc 1) (count-bits acc 2)))
            ((l (in-list layer*)))
    (define num-zeros (count-bits l 0))
    (if (or (not zeros) (< zeros num-zeros))
      (values l num-zeros)
      (values acc zeros))))

(define (part2 input)
  (error 'not-implemented))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-08"
    #:args (INPUT)
    (printf "part1: ~a~n" (part1 INPUT))
    (printf "part2: ~a~n" (part2 INPUT))
    (void)))
