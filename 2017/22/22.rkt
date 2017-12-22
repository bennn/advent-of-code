#lang racket

(define ITERS
  10000)

(define INPUT '(
"##########..#.###...##..#"
"##....#...#....#..####.#."
"#..#.##..#..##.###..#.###"
".#.#.......####.....#.#.."
"...######....#.##########"
"##.#.....#.#####.#....###"
"#.####.#..#.#.#...#.#..##"
"#.##..#####..###..###.##."
"#.####.#.##.##...#.#.#.##"
"#.#.#......##.##..###.#.#"
"#...#.#..#.##....#.##..##"
".#.....##.##..#.####..##."
".#......#.#.########..###"
"##....###.#.#.###...##..#"
"..##.###....#.....#...#.#"
"....##...##...##.##.#..##"
"..#.#.#..#######..###..##"
"......#####.#####..#.#..#"
".####.#......#..###..#.##"
"#....####.#..#.##.###.##."
"####.#...##....###...#.#."
"#####.#......#.#..###.##."
"#.##.#..#..#..#.....#.#.#"
"#...#.#.##.#.####.#.#..#."
".##.##..#..###.##.....###"
))

(define (clean? c)
  (eq? c #\.))

(define (infected? c)
  (eq? c #\#))

(define (turn-right dir)
  (case dir
   ((N) 'E)
   ((E) 'S)
   ((S) 'W)
   ((W) 'N)))

(define (turn-left dir)
  (case dir
   ((N) 'W)
   ((E) 'N)
   ((S) 'E)
   ((W) 'S)))

(define (go)
  (define BOARD
    (for/vector ((ln (in-list INPUT))) (for/vector ((c (in-string ln))) c)))

  (define NI 0)
  (define-syntax-rule (++ v)
    (set! v (+ 1 v)))
  (define num-bursts 0)
  (define (burst!)
    (printf "BURST~n")
    (set! num-bursts (+ num-bursts 1)))

  (define (node-at posn)
    (vector-ref (vector-ref BOARD (cdr posn)) (car posn)))

  (define (resize!)
    (define NR (vector-length BOARD))
    (define NC (vector-length (vector-ref BOARD 0)))
    (define NR2 (quotient NR 2))
    (define NC2 (quotient NC 2))
    (define NEW-BOARD
      (for/vector ((j (in-range (* NR 2))))
        (for/vector ((i (in-range (* NC 2))))
          (if (and (<= NR2 i) (< i NR) (<= NC2 j) (< j NC))
            (node-at (cons i j))
            #\.))))
    (set! BOARD NEW-BOARD))
  (define (maybe-resize! posn)
    (define NR (vector-length BOARD))
    (define NC (vector-length (vector-ref BOARD 0)))
    (if (or (= (car posn) 0) (= (cdr posn) 0) (= (car posn) (- NC 1)) (= (cdr posn) (- NR 1)))
      (begin (printf "RESIZE~n") (resize!)
        (cons (+ (car posn) NR) (+ (cdr posn) NC)))
      posn))

  (define (clean! posn)
    (vector-set! (vector-ref BOARD (cdr posn)) (car posn) #\.))
  (define (infect! posn)
    (++ NI)
    (vector-set! (vector-ref BOARD (cdr posn)) (car posn) #\#))
  (define (move-forward posn dir)
    (case dir
     ((N) (cons (car posn) (+ 1 (cdr posn))))
     ((E) (cons (+ 1 (car posn)) (cdr posn)))
     ((S) (cons (car posn) (- (cdr posn) 1)))
     ((W) (cons (- (car posn) 1) (cdr posn)))))
  (define (get-middle)
    (cons (quotient (vector-length (vector-ref BOARD 0)) 2) (quotient (vector-length BOARD) 2)))
  (let loop ((pre-posn (get-middle)) (dir 'S))
    (define posn
      (maybe-resize! pre-posn))
    (if (= num-bursts ITERS)
      (void)
      (let ()
    (define n (node-at posn))
    (define dir+
      (cond
       [(infected? n)
        (values (turn-right dir))]
       [else
        (values (turn-left dir))]))
    (burst!)
    (cond
     [(clean? n)
      (infect! posn)]
     [else
      (clean! posn)])
    (burst!)
    (loop (move-forward posn dir+) dir+))))
  (printf "part 1 : ~a~n" NI)
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-22"
    #:args ()
    (go)))
