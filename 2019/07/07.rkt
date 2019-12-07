#lang racket

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

(define (intcode input in*)
  (define state (file->state input))
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

(define (run-amp in* input memo)
  (or #;(hash-ref memo in* #f)
      (let ((v (intcode input in*)))
        #;(hash-set! memo in* v)
        v)))

(define (run-amp* in0 setting input memo)
  (for/fold ((acc in0))
            ((a (in-list setting)))
    (run-amp (list a acc) input memo)))

(define (part1 input)
  (define num-amp 5)
  (define max-phase 5)
  (define orig-in 0)
  (define memo (make-hash))
  (define best-setting #f)
  (define best-val
    (for*/fold ((acc #f))
               ((setting (in-permutations (range max-phase))))
      (define acc+ (run-amp* orig-in setting input memo))
      (if (or (not acc) (< acc acc+))
        (begin #;(set! best-setting setting)
               #;(printf "update ~a ~a~n" setting acc+)
               acc+)
        acc)))
  best-val)

(define (make-intcode-engine input in*)
  (define state (file->state input))
  (define out* (list))
  (define (make-continue pc)
    (lambda (more-in*)
      (set! in* (append in* more-in*))
      (loop pc)))
  (define (loop pc)
    (define-values [opcode param*] (parse-opcode (vector-ref state pc)))
    (cond
      [(= opcode 99)
       (values (if (null? out*) #f (car out*)) #f)]
      [(= opcode 1)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (apply + n*))
       (state-set! state loc res)
       (loop (+ pc 1 num-args))]
      [(= opcode 2)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (apply * n*))
       (state-set! state loc res)
       (loop (+ pc 1 num-args))]
      [(and (= opcode 3) (null? in*))
       (values (if (null? out*) #f (car out*)) (make-continue pc))]
      [(= opcode 3)
       (define in-val (begin0 (car in*) (set! in* (cdr in*))))
       (define num-args 1)
       (define instr (append (get-instr state pc num-args) (list in-val)))
       (define loc (vector-ref% state (+ pc 1)))
       (state-set! state loc in-val)
       (loop (+ pc 1 num-args))]
      [(= opcode 4)
       (define num-args 1)
       (define instr (get-instr state pc num-args))
       (define loc (state-ref state (+ pc 1) (car param*)))
       (set! out* (cons loc out*))
       (define next-pc (+ pc 1 num-args))
       #;(unless (or (zero? loc) (= 99 (let-values (((a b) (parse-opcode (vector-ref% state next-pc)))) a)))
         (raise-user-error 'intcode-engine "test failed at ~a~n curr instr ~a~n" loc instr))
       (loop next-pc)]
      [(= opcode 5)
       (define num-args 2)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) num-args param*))
       (if (not (zero? (car n*)))
         (loop (cadr n*))
         (loop (+ pc 1 num-args)))]
      [(= opcode 6)
       (define num-args 2)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) num-args param*))
       (if (zero? (car n*))
         (loop (cadr n*))
         (loop (+ pc 1 num-args)))]
      [(= opcode 7)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (if (apply < n*) 1 0))
       (state-set! state loc res)
       (loop (+ pc 1 num-args))]
      [(= opcode 8)
       (define num-args 3)
       (define instr (get-instr state pc num-args))
       (define n* (state-ref* state (+ pc 1) (- num-args 1) param*))
       (define loc (vector-ref% state (+ pc num-args)))
       (define res (if (apply = n*) 1 0))
       (state-set! state loc res)
       (loop (+ pc 1 num-args))]
      [else
        (raise-argument-error 'intcode-engine "opcode" opcode)]))
  (define-values [_ k] (loop 0))
  k)

(define (make-amp input in0)
  (make-intcode-engine input (list in0)))

(define (run-amp/feedback a in0)
  (a (list in0)))

(define (run-amp*/feedback orig-in setting* input)
  (define amp*
    (for/list ((a (in-list setting*)))
      (make-amp input a)))
  (let loop ((in-val orig-in)
             (amp* amp*))
    (if (null? amp*)
      in-val
      (let-values (((out-val amp+) (run-amp/feedback (car amp*) in-val)))
        (unless out-val (printf "out-val ~a~n" out-val))
        (loop out-val (append (cdr amp*) (if amp+ (list amp+) '())))))))

(define (part2 input)
  (define min-phase 5)
  (define max-phase 9)
  (define orig-in 0)
  (for/fold ((acc #f))
            ((setting (in-permutations (range min-phase (+ max-phase 1)))))
      (define acc+ (run-amp*/feedback orig-in setting input))
      (if (or (not acc) (< acc acc+)) acc+ acc)))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-07"
    #:args (INPUT)
    (printf "part1: ~a~n" (part1 INPUT))
    (printf "part2: ~a~n" (part2 INPUT))
    (void)))
