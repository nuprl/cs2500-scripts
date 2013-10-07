#lang racket
(require "config.rkt")
(provide grader
         grader-user
         grader-name
         grader-email

         graders
         symbol->grader)

;; string x string x string
;; must be read-able and write-able
(struct grader (user name email) #:prefab)

;; graders
;; -> (listof grader)
;; (listof grader) -> (void)
(define graders 
  (make-parameter 
    (map (lambda (ls)
           (grader (string->symbol (car (string-split (second ls) "@")))
             (car ls) (second ls)))
    (with-input-from-file graders-path read))))


;; symbol -> grader
;; produces a grader given a grader uername
(define (symbol->grader s)
  (car (filter (lambda (x) (eq? s (grader-user x))) (graders))))
