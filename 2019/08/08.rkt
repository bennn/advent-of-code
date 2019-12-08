#lang racket

(define w 25)
(define h 6)

(define (file->layer* w h img)
  (define img-bits (* w h))
  (define str (string-trim (file->string img)))
  (define len (string-length str))
  (let loop ((i 0))
    (if (<= len i)
      '()
      (cons (for*/vector ((row (in-range h))
                        (col (in-range w)))
              (define chr (string-ref str (+ i (+ col (* row w)))))
              (string->number (string chr)))
            (loop (+ i img-bits))))))

(module+
  test
  (require rackunit)
  (test-case "file->layer"
    (check-equal? (file->layer* 3 2 "test0") '(#(1 2 3 4 5 6) #(7 8 9 0 1 2)))))

(define (count-bits layer n)
  (for/sum ((i (in-vector layer))
            #:when (= i n))
    1))

(define (part1 input)
  (define layer* (file->layer* w h input))
  (for/fold ((acc #f)
             (zeros #f)
             #:result (* (count-bits acc 1) (count-bits acc 2)))
            ((l (in-list layer*)))
    (define num-zeros (count-bits l 0))
    (if (or (not zeros) (< num-zeros zeros))
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
