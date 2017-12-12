#lang racket

(require graph)

(define (go input)
  (define G
    (unweighted-graph/undirected
      (with-input-from-file input
        (lambda ()
          (append*
            (for/list ((ln (in-lines)))
              (define f+t (string-split (string-trim ln) " <-> "))
              (define f (string->number (car f+t)))
              (define t* (map string->number (string-split (cadr f+t) ", ")))
              (for/list ((t (in-list t*)))
                (list f t))))))))
  (define num-in-group
    (let-values (((a _b) (bfs G 0)))
      (for/sum ((v (in-hash-values a)))
        (if (integer? v) 1 0))))
  (define num-cc
    (length (cc G)))
  (printf "Solution:~n- part 1, num in group : ~a~n- part 2, num components : ~a~n" num-in-group num-cc)
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-12"
    #:args (INPUT)
    (go INPUT)))
