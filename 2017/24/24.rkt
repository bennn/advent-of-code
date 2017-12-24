#lang racket

(define (parse-edges input)
  (with-input-from-file input
    (lambda ()
      (for/list ((ln (in-lines)))
        (map string->number (string-split (string-trim ln) "/"))))))

(define (edge-lhs e)
  (car e))

(define (edge-rhs e)
  (cadr e))

(define (best-bridge lhs edge* part1?)
  (if (null? edge*)
    '()
    (let ()
      (define b*
        (for/list ((e (in-list edge*)) #:when (or (= lhs (edge-lhs e)) (= lhs (edge-rhs e))))
          (cond
           ((= lhs (edge-lhs e))
            (cons e (best-bridge (edge-rhs e) (remove e edge*) part1?)))
           ((= lhs (edge-rhs e))
            (define e+ (reverse e))
            (cons e+ (best-bridge (edge-lhs e) (remove e edge*) part1?)))
           (else (error 'die)))))
      (top-score* b* part1?))))

(define (top-score* b* part1?)
  (if (null? b*)
    '()
    (let ()
      (define-values [get-score score<]
        (if part1?
          (values bridge-score score<1)
          (values bridge-score2 score<2)))
      (for/fold ((BS #f) (BB #f) #:result BB)
                ((b (in-list b*)))
       (define S (get-score b))
       (if (or (eq? #f BB) (score< BS S))
         (values S b)
         (values BS BB))))))

(define (bridge-score2 b)
  (cons (length b) (bridge-score b)))

(define (score<1 s0 s1)
  (< s0 s1))

(define (score<2 s0 s1)
  (or (< (car s0) (car s1))
      (and (= (car s0) (car s1))
           (< (cdr s0) (cdr s1)))))

(define (bridge-score b*)
  (apply + (map (lambda (n*) (apply + n*)) b*)))

(define (go input)
  (define edge* (parse-edges input))
  (define part1
    (bridge-score (best-bridge 0 edge* #true)))
  (define part2
    (bridge-score (best-bridge 0 edge* #false)))
  (printf "part 1 : ~a~n" part1)
  (printf "part 2 : ~a~n" part2)
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-24"
    #:args (INPUT)
    (go INPUT)))
