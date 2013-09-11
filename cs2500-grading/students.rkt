#lang racket
(require "config.rkt")
(provide
  student
  student-username
  student-name
  student-email
  
  students

  symbol->students
  symbol->student
  string->students
  string->student)

;; string x string x string
(struct student (username name email) #:prefab)

;; students 
;; -> (listof student)
;; (listof student) -> void
(define students (make-parameter (read-students)))

;; read-students
;; -> (listof student)
;; reads and parses the list of students from students-path
(define (read-students)
  (let ([users-raw (with-input-from-file students-path read)])
    (map (lambda (raw) 
           (student (first raw)
                 (second (cdr raw))
                 (fourth (cdr raw)))) 
         users-raw)))

;; symbol->students
;; symbol -> (listof student)
;; given a symbol of usernames seperated by #\+, return the list of
;; students 
(define (symbol->students s) (string->students (symbol->string s)))

(define (string->students s)
  (let ([usernames (string-split s "+")])
    (filter (lambda (student) (memq (student-username student) usernames)) (students))))

;; symbol->student
;; assumes s represents a single username
(define (symbol->student s) (car (symbol->students s)))
(define (string->student s) (car (string->students s)))
