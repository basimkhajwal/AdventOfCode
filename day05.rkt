#lang racket

(require "common.rkt")

(define (uncons xs) (values (first xs) (rest xs)))

(define (match-units? a b) (and (char-ci=? a b) (not (char=? a b))))

(define (scan-solve acc input)
  (if (empty? input)
      (reverse acc)
      (if (empty? acc)
          (scan-solve (cons (first input) empty) (rest input))
          (let-values ([(a as) (uncons acc)]
                       [(x xs) (uncons input)])
            (if (match-units? a x)
                (scan-solve as xs)
                (scan-solve (cons x acc) xs))))))
            

(define (part-one input)
  (length (scan-solve empty (string->list input))))

(define (part-two input)

  (define input-list (string->list input))
  
  (define (without-unit unit)
    (filter (lambda (c) (not (char-ci=? c unit))) input-list))

  (define units (map (lambda (x) (integer->char (+ 65 x))) (range 26)))

  (foldl min (length input-list)
         (map
          (lambda (u) (length (scan-solve empty (without-unit u))))
          units)))

(define (solve)
  (define inputString (string-trim (file->string "input/05.txt")))

  (println "Day05")
  (printf "Task 1: ~a\n" (time (part-one inputString)))
  (printf "Task 2: ~a\n" (time (part-two inputString))))

(solve)
