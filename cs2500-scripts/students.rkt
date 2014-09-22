#lang racket
(require "config.rkt")
(provide
  student
  student-username
  student-passwdhash
  student-id
  student-name
  student-email
  student-section
  student-grader

  ;students
  read-students

  symbol->students
  symbol->student
  string->students
  string->student
  students->users.rktd)

;; string x string x number x string x string
(struct student (username passwdhash id name email section grader) #:prefab)

;; read-students
;; -> (listof student)
;; reads and parses the list of students from students-path
(define (read-students students-path)
  (let ([users-raw (with-input-from-file students-path read)])
    (map (lambda (raw)
           (student (car raw)
              (first (second raw))
              (third (second raw))
              (second (second raw))
              (fourth (second raw))
              (fifth (second raw))
              (sixth (second raw))))
         users-raw)))

;; students
;; -> (listof student)
;; (listof student) -> void
(define students (make-parameter '()#;(read-students students-path)))

;; symbol->students
;; symbol -> (listof student)
;; given a symbol of usernames seperated by #\+, return the list of
;; students
(define (symbol->students s) (string->students (symbol->string s)))

(define (string->students s)
  (let ([usernames (map string->symbol (string-split s "+"))])
    (filter (lambda (student) (memq (student-username student) usernames)) (students))))

;; symbol->student
;; assumes s represents a single username
(define (symbol->student s) (car (symbol->students s)))
(define (string->student s) (car (string->students s)))

(define (student->user student)
  `(,(student-username student)
     (,(format "~a" (student-passwdhash student))
      ,(format "~a" (student-name student))
      ,(format "~a" (student-id student))
      ,(format "~a" (student-email student))
      ,(format "~a" (student-section student))
      ,(format "~a" (student-grader student)))))

;; students->users.rktd
;; (listof student) -> (listof users)
;; creates a list of users that can be used as a users.rktd file for handin-server
(define (students->users.rktd students)
  (map student->user students))
