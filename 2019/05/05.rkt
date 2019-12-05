#lang racket

(module+ test (require rackunit))

(define (file->state fn)
  (let* ((str (string-trim (file->string fn)))
         (str* (string-split str ","))
         (v (for/vector ((s (in-list str*))) (string->number s))))
    v))

(define (integer->digit* i)
  (let loop ([acc '()]
             [prev 0]
             [m 1])
    (define m+ (* m 10))
    (define n (quotient (- (modulo i m+) prev) m))
    (define acc+ (cons n acc))
    (if (< i m+)
      acc+
      (loop acc+ (+ prev n) m+))))

(define (parse-opcode n)
  (define split-at 100)
  (define op (modulo n split-at))
  (define param* (reverse (integer->digit* (quotient n split-at))))
  (values op param*))

(module+
  test
  (test-case
    "parse-opcode"
    (check-equal? (call-with-values (lambda () (parse-opcode 1002)) cons)
                  (cons 2 '(0 1)))))

(define air-conditioner-id 1)
(define thermal-radiator-id 5)

(define (state-ref* state pc num-args param*)
  (for/list ((i (in-range num-args))
             (p (in-sequences (in-list param*) (in-cycle (in-value 0)))))
    (state-ref state (+ pc i) p)))

(define (state-ref state idx param)
  (define v0 (vector-ref% state idx))
  (case param
    ((0) (vector-ref% state v0))
    ((1) v0)
    (else (raise-argument-error 'state-ref "(integer-in 0 1)" 1 state idx param))))

(define (vector-ref% v i)
  (vector-ref v (modulo i (vector-length v))))

(define (vector-set!% v i x)
  (vector-set! v (modulo i (vector-length v)) x))

(define state-set! vector-set!%)

(define (get-instr state pc num-args)
  (cons
    (vector-ref% state pc)
    (for/list ((i (in-range num-args)))
      (vector-ref% state (+ pc 1 i)))))

(define (part1 input)
  (define state (file->state input))
  (define in* (list air-conditioner-id))
  (define out* (list))
  (let loop ((pc 0)
             (prev* '()))
    (define-values [opcode param*] (parse-opcode (vector-ref state pc)))
    (cond
      [(= opcode 99)
       (if (null? out*)
         #f
         (car out*))]
      [(= opcode 1)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (apply + n*))
       (state-set! state loc res)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [(= opcode 2)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (apply * n*))
       (state-set! state loc res)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [(= opcode 3)
       (define in-val (begin0 (car in*) (set! in* (cdr in*))))
       (define num-args 1)
       (define instr (append (get-instr state pc num-args) (list in-val)))
       (define loc (vector-ref% state (+ pc 1)))
       (state-set! state loc in-val)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [(= opcode 4)
       (define num-args 1)
       (define instr (get-instr state pc num-args))
       (define loc (state-ref state (+ pc 1) (car param*)))
       (set! out* (cons loc out*))
       (define next-pc (+ pc 1 num-args))
       (unless (or (zero? loc) (= 99 (let-values (((a b) (parse-opcode (vector-ref% state next-pc)))) a)))
         (raise-user-error 'part1 "test failed at ~a~n curr instr ~a~n prev instr ~a" loc instr (reverse prev*)))
       (loop next-pc (cons instr prev*))]
      [else
        (raise-argument-error 'part1 "opcode" opcode)])))

(define (part2 input)
  (define state (file->state input))
  (define in* (list thermal-radiator-id))
  (define out* (list))
  (let loop ((pc 0)
             (prev* '()))
    (define-values [opcode param*] (parse-opcode (vector-ref state pc)))
    (cond
      [(= opcode 99)
       (if (null? out*)
         #f
         (car out*))]
      [(= opcode 1)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (apply + n*))
       (state-set! state loc res)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [(= opcode 2)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (apply * n*))
       (state-set! state loc res)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [(= opcode 3)
       (define in-val (begin0 (car in*) (set! in* (cdr in*))))
       (define num-args 1)
       (define instr (append (get-instr state pc num-args) (list in-val)))
       (define loc (vector-ref% state (+ pc 1)))
       (state-set! state loc in-val)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [(= opcode 4)
       (define num-args 1)
       (define instr (get-instr state pc num-args))
       (define loc (state-ref state (+ pc 1) (car param*)))
       (set! out* (cons loc out*))
       (define next-pc (+ pc 1 num-args))
       (unless (or (zero? loc) (= 99 (let-values (((a b) (parse-opcode (vector-ref% state next-pc)))) a)))
         (raise-user-error 'part1 "test failed at ~a~n curr instr ~a~n prev instr ~a" loc instr (reverse prev*)))
       (loop next-pc (cons instr prev*))]

      [(= opcode 5)
       (define num-args 2)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) num-args param*))
       (if (not (zero? (car n*)))
         (loop (cadr n*) (cons instr prev*))
         (loop (+ pc 1 num-args) (cons instr prev*)))]
      [(= opcode 6)
       (define num-args 2)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) num-args param*))
       (if (zero? (car n*))
         (loop (cadr n*) (cons instr prev*))
         (loop (+ pc 1 num-args) (cons instr prev*)))]

      [(= opcode 7)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (if (apply < n*) 1 0))
       (state-set! state loc res)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [(= opcode 8)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (if (apply = n*) 1 0))
       (state-set! state loc res)
       (loop (+ pc 1 num-args) (cons instr prev*))]
      [else
        (raise-argument-error 'part1 "opcode" opcode)])))


(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-05"
    #:args (INPUT)
    #;(printf "part1 : ~a~n" (part1 INPUT))
    (printf "part2 : ~a~n" (part2 INPUT))
    (void)))
