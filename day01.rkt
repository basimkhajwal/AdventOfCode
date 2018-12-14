#lang racket

(require "common.rkt")

(define (partOne input) 
  (foldl + 0 input))

(define (partTwo input)
  (define seen (make-hash))
  (define (iter acc xs)
    (let* ([ys (if (empty? xs) input xs)]
           [acc2 (+ acc (first ys))])
      (if (hash-ref seen acc2 #f)
        acc2
        (begin
          (hash-set! seen acc2 #t)
          (iter acc2 (rest ys))))))
  (iter 0 input))

(define (file-to-nums input-file)
  (for/list ([i (in-lines input-file)]) (string->number i)))

((multipart-question "input/01.txt" file-to-nums #:name "Day 01") partOne partTwo)
