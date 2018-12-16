#lang racket

(require "common.rkt")

(define (max-by xs f [lt <])
  (define-values (a xs*) (values (first xs) (rest xs)))
  (define (step x y acc) (if (lt (cdr acc) y) (cons x y) acc))
  (car (foldl step (cons a (f a)) xs* (map f xs*))))

(define (max-hash-key hash #:by [f identity] [lt <])
  (car (max-by (hash->list hash) cdr)))

(define (max-hash-value hash) (apply max (cons 0 (hash-values hash))))

(define (part-one guard-sleep-times)
  (define (total-time-asleep sleep-hash)
    (for/sum ([cnt (in-hash-values sleep-hash)]) cnt))
  
  (define max-guard (max-by guard-sleep-times (compose total-time-asleep cdr)))
  
  (* (car max-guard) (max-hash-key (cdr max-guard))))

(define (part-two guard-sleep-times)
  (define max-guard
    (max-by guard-sleep-times (compose max-hash-value cdr)))
  (define max-time (max-hash-key (cdr max-guard)))
  (* (car max-guard) max-time))

(define (parse-file file)
  (define lines (sequence->list (in-lines file)))   

  (define (extract-date ln) (substring ln 0 19))

  (define (get-minute ln)
    (string->number (cadr (regexp-match #rx":([0-9]+)\\]" ln))))

  (define (get-guard-id ln)
    (string->number (cadr (regexp-match #rx"#([0-9]+)" ln))))
  
  (define (is-guard? ln) (string-contains? ln "Guard"))
  
  (define (split-guards xs)
    (if (empty? xs) empty
        (let-values ([(guard-items xs*) (splitf-at (rest xs) (compose not is-guard?))])
          (cons
           (cons (get-guard-id (first xs)) (map get-minute guard-items))
           (split-guards xs*)))))
  
  (define guard-data (split-guards (sort lines #:key extract-date string<?)))

  (define (collate-guards input)
    (define (collect guard guards)
      (hash-update guards (first guard)
                   (lambda (xs) (cons (rest guard) xs))
                   empty))
    (foldl collect (make-immutable-hash) input))

  (define (append-time-asleep change-times sleep-hash)
    (define next-sleep (if (empty? change-times) 60 (first change-times)))
    (define next-awake (if (< (length change-times) 2) 60 (cadr change-times)))
    (define sleep-hash*
      (for/fold ([current-hash sleep-hash])
                ([t (in-range next-sleep next-awake)])
        (hash-update current-hash t add1 0)))
    (if (< next-awake 60)
        (append-time-asleep (drop change-times 2) sleep-hash*)
        sleep-hash))

  (define (time-asleep guard-times)
    (for/fold ([sleep-hash (make-immutable-hash)])
              ([change-times (in-list guard-times)])
      (append-time-asleep change-times sleep-hash)))

  (define guard-sleep-times
    (hash-map (collate-guards guard-data)
              (lambda (g xs) (cons g (time-asleep xs)))))
  guard-sleep-times)

((multipart-question "input/04.txt" parse-file #:name "Day04") part-one part-two)