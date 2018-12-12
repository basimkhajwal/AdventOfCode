#lang racket

;; Common utlities that I think of along the way...

(provide process-file
         multipart-question)

(define (process-file file-name process)
  (define input-file (open-input-file file-name))
  (define result (process input-file))
  (close-input-port input-file)
  result)

(define (multipart-question file-name extract-data #:name [question-name "AdventOfCode"])
  (define input (process-file file-name extract-data))
  (lambda parts (begin
    (printf "~a:\n" question-name)
    (for ([i '("One" "Two" "Three" "Four" "Five")] ; assuming no more than 5 parts :D
          [p parts])
      (printf "Part ~a: ~a\n" i (p input))))))