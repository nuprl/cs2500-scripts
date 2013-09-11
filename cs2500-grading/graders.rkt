#lang racket
(require "config.rkt")
(provide grader
         grader-user
         grader-name
         grader-email

         graders)

;; string x string x string
;; must be read-able and write-able
(struct grader (user name email) #:prefab)

;; graders
;; -> (listof grader)
;; (listof grader) -> (void)
(define graders 
  (make-parameter 
    (map (lambda (ls)
           (grader (car (string-split (second ls) "@"))
             (car ls) (second ls)))
    (with-input-from-file graders-path read))))


