#lang racket
;; TODO: test
(require "config.rkt")
(provide problem-set
         problem-set-name
         problem-set-dir

         grader
         grader-user
         grader-name
         grader-email

         graders

         grade-entry
         grade-entry-grader
         grade-entry-user
         grade-entry-ps
         grade-entry-grade

         user
         user-username
         user-name
         user-email

         symbol->users
         symbol->user
         string->users
         string->user
         user->grade
         read-gradebook
         append-to-gradebook!)

;; name (string) x dir (string)
(struct problem-set (name dir) #:prefab)
;; user (string) x name (string) x email (string)
(struct grader (user name email) #:prefab)
;; grader x string x problem-set x number
(struct grade-entry (grader user ps grade) #:prefab)
;; string x string x string
(struct user (username name email) #:prefab)

;; (listof grader)
(define graders 
  (make-parameter 
    (map (lambda (triple) (grader (car (string-split (second triple) "@"))
                                  (car triple) (second triple)))
    (with-input-from-file graders-path read))))

;; -> (listof grade-entry)
(define (read-gradebook) 
  (with-input-from-file gradebook-path read))

(define (save-gradebook! grades)
  (with-output-to-file gradebook-path (write grades) #:exists
  'truncate/replace))

;; append-to-gradebook!
;; (listof grade-entry) -> void
;; appends grades to gradebook, and saves the gradebook file to disk
(define (append-to-gradebook! grades) 
  (save-gradebook! (append grades (read-gradebook))))

(define (read-users)
  (let ([users-raw (with-input-from-file users-path read)])
    (map (lambda (raw) 
           (user (first raw)
                 (second (cdr raw))
                 (fourth (cdr raw)))) 
         users-raw)))

;; symbol->users
;; symbol -> (listof user)
;; given a symbol of usernames seperated by #\+, return the list of
;; users 
(define (symbol->users s) (string->users (symbol->string s)))

(define (string->users s)
  (let ([usernames (string-split s "+")])
    (filter (lambda (user) (memq (user-username user) usernames)) (read-users))))

;; symbol->user
;; assumes s represents a single username
(define (symbol->user s) (car (symbol->users s)))
(define (string->user s) (car (string->users s)))

;; user->grade
;; user -> number
;; gives the numeric average grade of the user
(define (user->grade u)
  (average-grade 
    (filter (lambda (grade) 
            (equal? (grade-entry-user grade) u))
    (read-gradebook))))

;; average-grade
;; (listof grade-entry) -> number
(define (average-grade grades)
  (let ([grades (map grade-entry-grade grades)])
    (/ (apply + grades) (length grades))))
