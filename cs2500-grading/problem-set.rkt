#lang racket
;; TODO: test
(require 
  racket/date
  "config.rkt")
(provide
  problem-set
  problem-set-name
  problem-set-dir
  problem-set-start-date
  problem-set-end-date

  problem-sets
  
  active-problem-sets
  
  refresh-problem-sets
  write-problem-sets!
  write-current-problem-sets!

  gen-problem-sets)

;; name (string) x dir (string) x exact-integer x exact-integer
;; must be read-able and write-able
(struct problem-set (name dir start-date end-date) #:prefab)

(define problem-sets 
  (make-parameter (with-input-from-file (problem-sets-path) read)))

;; refresh-problem-sets
;; (listof problem-set) -> (listof problem-set) (listof problem-set)
;; given a list of all problem sets, returns a list of active problem
;; sets (first value) and a list of inactive problem sets (second value)
(define (refresh-problem-sets pss)
  (let* ([date (date->seconds (current-date))]
         [active (active-problems pss)]
         [inactive (remove* active pss)])
    (values active inactive)))

(define (active-problems pss)
  (filter (lambda (ps) 
                  (and (>= date (problem-set-start-date ps))
                    (< date (problem-set-end-date ps))))
          pss))

(module+ test
  (require rackunit)
  (define psls (gen-problem-sets (date->seconds (current-date)) 5))
  (pretty-write psls)
  (check-equal? (length psls) 5)
  (check-true (andmap problem-set? psls))
  (let-values ([(active inactive) (refresh-problem-sets psls)])
    (check-equal? (length active) 1)
    (check-equal? (length inactive) 4)) 
  )

;; write-problem-sets!
;; (listof problem-set) (listof problem-set) -> (void)
;; writes the active and inactive sets to the server configuration file
(define (write-problem-sets! active inactive)
  (let ([conf (filter (lambda (x) (not (memq (car x) '(active-dirs inactive-dirs)))) (with-input-from-file server-config-path read))])
    (with-output-to-file 
      server-config-path
      (thunk (pretty-write 
               (append (list `(active-dirs ,(map problem-set-dir active))
                  `(inactive-dirs ,(map problem-set-dir inactive)))
                       conf)))
      #:exists 'replace)))
;; write-current-problem-sets!
;; (void) -> (void)
;; refreshs the problem sets stored in problem-sets-path, and
;; writes them to the server config file.
(define (write-current-problem-sets!)
  (call-with-values 
    (thunk (refresh-problem-sets (with-input-from-file (problem-sets-path) read)))
    write-problem-sets!))

;; gen-problem-sets
;; exact-integer natrual -> (listof problem-set)
;; generates problem sets with name and dir (format "ps~a" i) for i = 1
;; to n. The first problem set will become active on date d, and
;; remain active until exactly 7 days later
(define (gen-problem-sets d n)
  (let ([names/dirs (build-list n (compose (curry format "ps~a") add1))]
        [start-dates (build-list n (lambda (n) (add-days d (* n 7))))]
        [end-dates (build-list n (lambda (n) (add-days d (* (add1 n) 7))))])
    (map (lambda (name start end) (problem-set name name start end))
      names/dirs
      start-dates
      end-dates)))

(define (add-days d days)
  (+ d (* days 24 60 60)))
