#lang racket

(require "common.rkt")

(struct elf (id x y width height))

(define (parse-elf line)
  (define parts (string-split (string-replace line (regexp ":|,|@|x|#") " ")))
  (apply elf (map string->number parts)))

(define (part-one input)
  (define square-counts (make-hash))
  (define ans 0)
  (for* ([e input]
         [x (in-range (elf-x e) (+ (elf-x e) (elf-width e)))]
         [y (in-range (elf-y e) (+ (elf-y e) (elf-height e)))])
    (let ([p (+ (* 1000 x) y)])
      (hash-update! square-counts p add1 0)
      (if (= (hash-ref square-counts p) 2)
          (set! ans (add1 ans))
          void)))
  ans)

(define (part-two input)
  (define (range-overlap start1 len1 start2 len2)
    (and (< start1 (+ start2 len2)) (< start2 (+ start1 len1))))
  
  (define (overlaps a b)
    (and
     (range-overlap (elf-x a) (elf-width a) (elf-x b) (elf-width b))
     (range-overlap (elf-y a) (elf-height a) (elf-y b) (elf-height b))))
  
  (define (has-overlap a) (for/first ([b input] #:unless (eq? a b) #:when (overlaps a b)) #t))

  (for/first ([a input] #:when (not (has-overlap a))) (elf-id a)))

(define (parse-file input-file)
  (for/list ([ln (in-lines input-file)])
    (parse-elf ln)))

((multipart-question "input/03.txt" parse-file #:name "Day03") part-one part-two)