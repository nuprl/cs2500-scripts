#lang racket
;; TODO: test
(require 
  "config.rkt"
  "graders.rkt"
  "students.rkt"
  "problem-set.rkt")
(provide
  grade-entry
  grade-entry-grader
  grade-entry-student
  grade-entry-ps
  grade-entry-grade

  student->grade
  read-gradebook
  append-to-gradebook!)

;; grader x string x problem-set x number
(struct grade-entry (grader student ps grade) #:prefab)

;; read-gradebook
;; -> (listof grade-entry)
;; reads the gradebook from the gradebook-path
(define (read-gradebook) 
  (with-input-from-file gradebook-path read))

;; save-gradebook!
;; (listof grade-entry) -> (void)
;; saves a gradebook to the gradebook-path, overwriting any current
;; gradebook.
(define (save-gradebook! gradebook)
  (with-output-to-file gradebook-path (write grades) #:exists
  'truncate/replace))

;; append-to-gradebook!
;; (listof grade-entry) -> void
;; appends grades to gradebook, and saves the gradebook file to disk
(define (append-to-gradebook! grades) 
  (save-gradebook! (append grades (read-gradebook))))

;; student->grade
;; student -> number
;; computes the average grade for a student
(define (student->grade student)
  (average-grade 
    (filter (lambda (grade) 
              (equal? (grade-entry-student grade) student))
      (read-gradebook))))

;; average-grade
;; (listof grade-entry) -> number
(define (average-grade grades)
  (let ([grades (map grade-entry-grade grades)])
    (/ (apply + grades) (length grades))))
