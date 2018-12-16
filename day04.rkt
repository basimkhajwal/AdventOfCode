#lang racket

(require "common.rkt")

;(define (make-my-date year month day hour minute)
;  (make-date 0 minute hour day month year 6 348 #f 0))
;
;(define (get-date-time ln)
;  (define m (regexp-match #rx"^\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\]" ln))
;  (apply make-my-date (map string->number (rest m))))

(define (max-by xs f [lt <])
  (define-values (a xs*) (split-at xs 1))
  (define (step x y acc) (if (lt (cdr acc) y) (cons x y) acc))
  (car (foldl step (cons (car a) (f (car a))) xs* (map f xs*))))

(define (part-one input)

  (define (append-time-asleep change-times sleep-hash)
    (define next-sleep (if (empty? change-times) 60 (first change-times)))
    (define next-awake (if (< (length change-times) 2) 60 (cadr change-times)))
    (for ([t (in-range next-sleep next-awake)])
      (hash-update! sleep-hash t add1 0))
    (if (< next-awake 60)
        (append-time-asleep (drop change-times 2) sleep-hash)
        void))

  (define (time-asleep guard-times)
    (define sleep-hash (make-hash))
    (for ([change-times (in-list guard-times)]) (append-time-asleep change-times sleep-hash))
    (sleep-hash))

  (define (total-time-asleep sleep-hash)
    (for/sum ([cnt (in-hash-values sleep-hash)]) cnt))

  (define (collect guard guards)
    (hash-update guards (first guard)
                 (lambda (xs) (cons (rest guard) xs))
                 empty))

  (define collected-guards (foldl collect (make-immutable-hash) input))

  (define guard-sleep-times void)
  ; TODO finish this
  "")

  

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
  
  (split-guards (sort lines #:key extract-date string<?)))

((multipart-question "input/04.txt" parse-file #:name "Day04") part-one)