#lang racket

(define INPUT
  "R4, R1, L2, R1, L1, L1, R1, L5, R1, R5, L2, R3, L3, L4, R4, R4, R3, L5, L1, R5, R3, L4, R1, R5, L1, R3, L2, R3, R1, L4, L1, R1, L1, L5, R1, L2, R2, L3, L5, R1, R5, L1, R188, L3, R2, R52, R5, L3, R79, L1, R5, R186, R2, R1, L3, L5, L2, R2, R4, R5, R5, L5, L4, R5, R3, L4, R4, L4, L4, R5, L4, L3, L1, L4, R1, R2, L5, R3, L4, R3, L3, L5, R1, R1, L3, R2, R1, R2, R2, L4, R5, R1, R3, R2, L2, L2, L1, R2, L1, L3, R5, R1, R4, R5, R2, R2, R4, R4, R1, L3, R4, L2, R2, R1, R3, L5, R5, R2, R5, L1, R2, R4, L1, R5, L3, L3, R1, L4, R2, L2, R1, L1, R4, R3, L2, L3, R3, L2, R1, L4, R5, L1, R5, L2, L1, L5, L2, L5, L2, L4, L2, R3")

(define dir (box 'N))
(define moves (make-hash))
(define seen (mutable-set))
(define dub (box #f))

(define (register-loc! loc)
  (unless (unbox dub)
    (if (set-member? seen loc)
      (set-box! dub loc)
      (set-add! seen loc))))

(define (turn! lr)
  (set-box! dir
    (case (unbox dir)
     ((N) (if (eq? lr 'L) 'W 'E))
     ((E) (if (eq? lr 'L) 'N 'S))
     ((S) (if (eq? lr 'L) 'E 'W))
     ((W) (if (eq? lr 'L) 'S 'N))
     (else (error 'turn! "bad ~a ~a~n" (unbox dir) lr)))))

(define (parse-instruction str)
  (values (string->symbol (string (string-ref str 0)))
          (string->number (substring str 1))))

(define (go input)
  (for ((instr (in-list (string-split INPUT ", "))))
    (define-values [lr amt] (parse-instruction instr))
    (turn! lr)
    (for ((i (in-range amt)))
      (hash-update! moves (unbox dir) (lambda (old) (+ old 1)) (lambda () 0))
      (define loc (get-location moves))
      (register-loc! loc)))
  (printf "part 1 : ~a~n" (abs (apply + (get-location moves))))
  (printf "part 2 : ~a~n" (abs (apply + (unbox dub))))
  (void))

(define (get-location moves)
  (list (- (hash-ref moves 'N 0) (hash-ref moves 'S 0))
        (- (hash-ref moves 'E 0) (hash-ref moves 'W 0))))

(module+ main
  (go INPUT))
