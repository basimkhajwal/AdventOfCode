#lang racket

(require "common.rkt")

(define file-name "input/01.txt")

(define (partOne input) 
  (foldl + 0 input))

(define (partTwo input)
  (define seen (make-hash))
  (define (iter acc xs)
    (if (empty? xs) (iter acc input)
      (let ([acc2 (+ acc (first xs))])
        (if (hash-ref seen acc2 #f) acc2
          (begin
            (hash-set! seen acc2 #t)
            (iter acc2 (rest xs)))))))
  (iter 0 input))

(define (file-to-nums input-file)
  (for/list ([i (in-lines input-file)]) (string->number i)))

((multipart-question file-name file-to-nums #:name "Day 01") partOne partTwo)