#lang racket

(require graph)

(define (file->edge* input)
  (with-input-from-file input
    (lambda ()
      (for/list ((ln (in-lines)))
        (string-split (string-trim ln) ")")))))

(define (part1 input)
  (define G (unweighted-graph/directed (file->edge* input)))
  (define-values [dist _] (bellman-ford G "COM"))
  (apply + (hash-values dist)))

(define (part2 input)
  (define G (unweighted-graph/undirected (file->edge* input)))
  (define pt (fewest-vertices-path G "YOU" "SAN"))
  (- (length pt) 3))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-06"
    #:args (INPUT)
    (printf "part1: ~a~n" (part1 INPUT))
    (printf "part2: ~a~n" (part2 INPUT))
    (void)))
