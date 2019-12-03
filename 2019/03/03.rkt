#lang racket

;; wire -> setof point
(define (parse fn)
  (apply values
  (for/list ((ln (in-list (file->lines fn))))
    (parse-wire ln))))

(define (parse2 fn)
  (apply values
  (for/list ((ln (in-list (file->lines fn))))
    (parse-wire2 ln))))

(define (parse-dir str)
  (values
    (string->symbol (substring str 0 1))
    (string->number (substring str 1))))

(define (K a x) a)

(define (parse-d sym)
  (case sym
    ((U) (values values add1))
    ((D) (values values sub1))
    ((L) (values sub1 values))
    ((R) (values add1 values))
    (else (raise-user-error 'parse-d "bad input ~a" sym))))

(define (parse-d2 sym)
  (case sym
    ((U) (values K +))
    ((D) (values K -))
    ((L) (values - K))
    ((R) (values + K))
    (else (raise-user-error 'parse-d2 "bad input ~a" sym))))

(define origin (cons 0 0))

(define (parse-wire ln)
  (define pt* (make-hash (list (cons origin #t))))
  (define dir* (string-split (string-trim ln) ","))
  (for/fold ((acc origin))
            ((dir (in-list dir*)))
    (define-values [d mag] (parse-dir dir))
    (define-values [dx dy] (parse-d d))
    (for/fold ((acc acc))
              ((i (in-range mag)))
      (define acc+ (cons (dx (car acc)) (dy (cdr acc))))
      (hash-set! pt* acc+ #t)
      acc+))
  pt*)

(define (parse-wire2 ln)
  (for/fold ((acc (list origin))
             #:result (reverse acc))
            ((x* (in-list (string-split (string-trim ln) ","))))
    (define-values [d mag] (parse-dir x*))
    (define-values [dx dy] (parse-d2 d))
    (define prev (car acc))
    (cons (cons (dx (car prev) mag) (dy (cdr prev) mag)) acc)))

(define (dist0 pt)
  (+ (abs (car pt)) (abs (cdr pt))))

(define (origin? pt)
  (equal? origin pt))

(define (part1-0 input)
  (define-values [w0 w1] (parse input))
  (define int*
    (for*/list ((k0 (in-hash-keys w0))
                (k1 (in-hash-keys w1))
                #:when (and (not (origin? k0))
                            (equal? k0 k1)))
      k0))
  (printf "part1 : ~a~n" (dist0 (argmin dist0 int*))))

(define (best-of a b)
  (argmin dist0 (list a b)))

(define (in-line* pt*)
  (define acc (box pt*))
  (define (next!)
    (and (not (null? (cdr (unbox acc))))
         (begin0 (cons (car (unbox acc)) (cadr (unbox acc)))
                 (set-box! acc (cdr (unbox acc))))))
  (in-producer next! #f))

(define (intersect* w0 w1*)
  (for/fold ((acc '()))
            ((w1 (in-line* w1*)))
    (define x (line-intersect w0 w1))
    (if (and x (not (equal? origin x))) (cons x acc) acc)))

(define (line-intersect l0 l1)
  (and (perp? l0 l1)
       (let-values (((vl hl) (vert/horiz l0 l1)))
         (and (overlap? vl hl)
              (cons (car (car vl)) (cdr (car hl)))))))

(define (perp? l0 l1)
  (or (and (vert? l0)
           (horiz? l1))
      (and (horiz? l0)
           (vert? l1))))

(define (vert? ln)
  (= (car (car ln)) (car (cdr ln))))

(define (horiz? ln)
  (= (cdr (car ln)) (cdr (cdr ln))))

(define (overlap? vl hl)
  (and (<= (caar hl) (caar vl) (cadr hl))
       (<= (cdar vl) (cdar hl) (cddr vl))))

(define (vert/horiz l0 l1)
  (if (vert? l0)
    (values (norm-line l0) (norm-line l1))
    (values (norm-line l1) (norm-line l0))))

(define (norm-line ln)
  (define-values [x0 y0 x1 y1] (line-split ln))
  (cons (cons (min x0 x1)
              (min y0 y1))
        (cons (max x0 x1)
              (max y0 y1))))

(define (line-split ln)
  (values (caar ln) (cdar ln) (cadr ln) (cddr ln)))

(define (part1 input)
  (define-values [w0* w1*] (parse2 input))
  (define x
    (for/fold ((acc #f))
              ((w0 (in-line* w0*)))
      (define pt* (intersect* w0 w1*))
      (if (null? pt*)
        acc
        (argmin dist0 (if acc (cons acc pt*) pt*)))))
  (printf "part1 : ~a~n" (dist0 x)))

(define (make-dist-score input)
  (define-values [seg0* seg1*]
    (apply values
           (for/list ((ln (in-list (file->lines input))))
             (string-split (string-trim ln) ","))))
  (lambda (xy)
    (+ (walk-score xy seg0*)
       (walk-score xy seg1*))))

(define (point-on-line? xy ln)
  (if (vert? ln)
    (and (= (car xy) (caar ln))
         (<= (cdar ln) (cdr xy) (cddr ln)))
    (and (= (cdr xy) (cdar ln))
         (<= (caar ln) (car xy) (cadr ln)))))

(define (walk-score xy seg*)
  (let loop ((curr-loc origin)
             (curr-mag 0)
             (seg* seg*))
    (cond
      [(null? seg*)
       (raise-user-error 'walk-score "stuck")]
      [else
       (define-values [d mag] (parse-dir (car seg*)))
       (define-values [dx dy] (parse-d2 d))
       (define next-loc (cons (dx (car curr-loc) mag) (dy (cdr curr-loc) mag)))
       (define ln (norm-line (cons curr-loc next-loc)))
       (if (point-on-line? xy ln)
         (+ curr-mag
            (case d
              ((U D) (abs (- (cdr xy) (cdr curr-loc))))
              ((L R) (abs (- (car xy) (car curr-loc))))
              (else (error 'die))))
         (loop next-loc (+ curr-mag mag) (cdr seg*)))])))

(define (part2 input)
  (define-values [w0* w1*] (parse2 input))
  (define dist-score (make-dist-score input))
  (define x
    (for/fold ((acc #f))
              ((w0 (in-line* w0*)))
      (define score* (map dist-score (intersect* w0 w1*)))
      (if (null? score*)
        acc
        (if (not acc)
          (apply min score*)
          (apply min (cons acc score*))))))
  (printf "part2 : ~a~n" x))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-03"
    #:args (INPUT)
    (part1 INPUT)
    (part2 INPUT)
    (void)))
