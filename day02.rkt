#lang racket

(require "common.rkt")

(define (part-one lines)
  (define (frequencies xs)
    (define (step letter freq-table)
      (hash-update freq-table letter add1 0))
    (foldl step (make-immutable-hash) xs))

  (define (has-repeat xs n)
    (define f (frequencies xs))
    (define f* (frequencies (map cdr (hash->list f))))
    (hash-has-key? f* n))
  
  (define (count-repeats n)
    (length (filter (lambda (line)
                      (has-repeat (string->list line) n))
                    lines)))
  
  (* (count-repeats 2) (count-repeats 3)))

(define (part-two lines)

  (define (count-different as bs)
    (for/sum ([a (in-string as)]
              [b (in-string bs)])
      (if (equal? a b) 0 1)))

  (define (common-chars as bs)
    (list->string
     (for/list ([a (in-string as)]
                [b (in-string bs)]
                #:when (equal? a b))
       a)))
  
  (define correct
    (for*/first ([a lines]
                 [b lines]
                 #:when (= (count-different a b) 1))
      (cons a b)))
  
  (common-chars (car correct) (cdr correct)))

((multipart-question "input/02.txt"
                     (lambda (fin) (sequence->list (in-lines fin)))
                     #:name "Day02") part-one part-two)