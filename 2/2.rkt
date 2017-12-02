#lang racket
(require math/number-theory)


(define (parse ln)
  (with-input-from-string (string-append "( " ln " )")
    read))

(define (find-max/min num*)
  (values (apply max num*) (apply min num*)))

(define (find-divides num*)
  (let loop ([n* num*])
    (cond
     [(null? n*)
      (error 'die)]
     [else
      (define x (car n*))
      (define y
        (for/or ((n (in-list (cdr n*))))
          (if (or (divides? x n) (divides? n x)) n #f)))
      (if y
        (values (max y x) (min y x))
        (loop (cdr n*)))])))

(define (process-file input [part2? #f])
  (define f (if part2? find-divides find-max/min))
  (with-input-from-file input
    (lambda ()
      (for/sum ((ln (in-lines)))
        (define-values [large small] (f (parse ln)))
        (quotient large small)))))

(module+ main
  (process-file
    (vector-ref (current-command-line-arguments) 0)
    #true))

