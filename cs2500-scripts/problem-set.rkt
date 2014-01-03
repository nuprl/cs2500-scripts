#lang racket
;; TODO: test
(require
  racket/date
  "config.rkt")
(provide
  problem-set
  problem-set-name
  problem-set-dir

  write-problem-sets!

  gen-problem-sets)

;; name (string) x dir (string)
;; must be read-able and write-able
(struct problem-set (name dir) #:prefab)

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

;; gen-problem-sets
;; exact-integer natrual -> (listof problem-set)
;; generates problem sets with name and dir (format "ps~a" i) for i = 1
;; to n.
(define (gen-problem-sets d n #:prefix [prefix "hw"])
  (let ([names/dirs (build-list n (compose (curry format "~a~a" prefix)
                                           (lambda (x) (~a x #:width 2
                                                           #:align 'right
                                                           #:left-pad-string "0"))
                                           add1))])
    (map (lambda (name) (problem-set name name)) names/dirs)))
