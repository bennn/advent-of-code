#lang racket

(define (file->pos fn)
  (let* ((str (string-trim (file->string fn)))
         (str* (string-split str ","))
         (v (for/vector ((s (in-list str*))) (string->number s))))
    v))

(define (part1-update! v)
  (nv-update! v 12 2))

(define (nv-update! v n0 n1)
  (vector-set! v 1 n0)
  (vector-set! v 2 n1)
  (void))

(define (vector-ref% v i)
  (vector-ref v (modulo i (vector-length v))))

(define (vector-set!% v i x)
  (vector-set! v (modulo i (vector-length v)) x))

(define (get-op v pos)
  (apply
    values
    (for/list ((i (in-range 4)))
      (vector-ref% v (+ pos i)))))

(define (opcode->f op)
  (case op
    ((1) +)
    ((2) *)
    (else #f)))

(define (next-pos v pos)
  (modulo (+ pos 4) (vector-length v)))

(define (part1-run! v [pos 0])
  ;; (printf "step~n")
  (define-values [op l0 l1 lr] (get-op v pos))
  (cond
    [(= op 99)
     (void)]
    [(member op '(1 2))
     (define f (opcode->f op))
     (define n0 (vector-ref% v l0))
     (define n1 (vector-ref% v l1))
     (define nr (f n0 n1))
     (vector-set!% v lr nr)
     (part1-run! v (next-pos v pos))]
    [else
     (raise-user-error 'part1 "unknown opcode ~a" op)]))


(define (part1 fn)
  (define v (file->pos fn))
  (part1-update! v)
  (part1-run! v)
  (printf "part1 : ~a~n" (vector-ref v 0)))

(define (part2 fn)
  (define magic-result 19690720)
  (printf "part2 : ~a~n"
    (for*/or ((n0 (in-range 100))
              (n1 (in-range 100)))
      (define v (file->pos fn))
      (nv-update! v n0 n1)
      (part1-run! v)
      (and (= (vector-ref v 0) magic-result)
           (+ (* 100 n0) n1)))))

(module+
  main
  (require racket/cmdline)
  (command-line
    #:args (fn)
    ;(part1 fn)
    (part2 fn)
    (void)))
