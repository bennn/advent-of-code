#lang racket/base

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "aoc-new"
    #:args (pre-NUM)
    (let ([NUM (if (= 1 (string-length pre-NUM)) (string-append "0" pre-NUM) pre-NUM)])
      (unless (directory-exists? NUM)
        (make-directory NUM)
        (with-output-to-file (path-add-extension (build-path NUM NUM) ".rkt")
          (lambda ()
            (printf "#lang racket~n")
            (printf "~n")
            (printf "(define (part1 input)~n")
            (printf "  (error 'not-implemented))~n")
            (printf "~n")
            (printf "(define (part2 input)~n")
            (printf "  (error 'not-implemented))~n")
            (printf "~n")
            (printf "(module+ main~n")
            (printf "  (require racket/cmdline)~n")
            (printf "  (command-line~n")
            (printf "    #:program \"day-~a\"~n" NUM)
            (printf "    #:args (INPUT)~n")
            (printf "    (part1 INPUT)~n")
            (printf "    (part2 INPUT)~n")
            (printf "    (void)))~n" )))))))
