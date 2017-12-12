#lang racket

(define (go input)
  (with-input-from-file input
    (lambda () 

      (for/sum ((ln (in-lines)))
        (if (valid-phrase2? ln) 1 0)))))

(define (valid-phrase? ln)
  (define w* (string-split (string-trim ln)))
  (= (length w*) (length (remove-duplicates w*))))

(define (valid-phrase2? ln)
  (define w* (string-split (string-trim ln)))
  (let loop ((w* w*))
    (if (null? w*)
      #true
      (let ((a (car w*))
            (b (cdr w*)))
        (and (no-anagrams? a b)
             (loop b))))))

(define (no-anagrams? a b*)
  (for/and ((b (in-list b*)))
    (not (anagram?  a b))))

(define (anagram? a b)
  (equal? (sort (string->list a) char<?)
    (sort (string->list b) char<?)))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "day-4"
    #:args (INPUT)
    (go INPUT)))
