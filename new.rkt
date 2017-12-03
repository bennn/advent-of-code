#lang racket/base

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "aoc-new"
    #:args (NUM)
    (begin
      (unless (directory-exists? NUM)
        (make-directory NUM)
        (with-output-to-file (path-add-extension (build-path NUM NUM) ".rkt")
          (lambda ()
            (printf "#lang racket~n")
            (printf "~n")
            (printf "(define (go input)~n")
            (printf "  (error 'not-implemented))~n")
            (printf "~n")
            (printf "(module+ main~n")
            (printf "  (require racket/cmdline)~n")
            (printf "  (command-line~n")
            (printf "    #:program \"day-~a\"~n" NUM)
            (printf "    #:args (INPUT)~n")
            (printf "    (go INPUT)))~n" )))))))
