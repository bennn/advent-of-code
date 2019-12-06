#lang racket

(require graph)

(define (part1 input)
  (define G
    (unweighted-graph/directed
      (with-input-from-file
        input
        (lambda ()
          (for/list ((ln (in-lines)))
            (string-split (string-trim ln) ")"))))))
  (define-values [dist _] (bellman-ford G "COM"))
  (apply + (hash-values dist)))

(define (part2 input)
  (define edge*
      (with-input-from-file
        input
        (lambda ()
          (for/list ((ln (in-lines)))
            (string-split (string-trim ln) ")")))))
  (define G
    (unweighted-graph/undirected
      edge*))
  (define pt (fewest-vertices-path G "YOU" "SAN"))
  (- (length pt) 3) #;
  (let loop ((pt pt))
    (if (or (null? pt) (null? (cdr pt)))
      0
      (let ((a (car pt))
            (x (cadr pt))
            (tl (cdr pt)))
        (+ (if (member (list x a) edge*) 1 0)
           (loop tl))))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-06"
    #:args (INPUT)
    (printf "part1: ~a~n" (part1 INPUT))
    (printf "part2: ~a~n" (part2 INPUT))
    (void)))
