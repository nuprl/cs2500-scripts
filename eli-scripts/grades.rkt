#lang racket/base

(define grades-conf
  ;;                      01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
  ;;                      -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  ;;            2011sp     6  8  9 12 13 14 13  4  5 11 13 12 12 13  7
  ;;            2011fa     3  9 11  6 15 13 12  3 14  5 10 10 13 11 10
  ;;            2012sp     2  4  8 11 12 15 14  9 10  9 15 14 14 16 13
  ;;            2013fa     3 10  6 12 12 12  8  8 10 14  8 13 13 13  8
  ;;                       3  7 10 12 12 13  7 11 10 11 12
  `([hw    "Homework" ,(+ 12 20 28 40 30) (12 20 28 40 30 60)]
    [exam   "Exam"     60 (60)]
    [honors-exam   "Honors Exam Supplement"     42 (42)]
    [quiz  "Quiz"     4 (1 1 1 1)]
    [honors-hw "Honors Homework" ,(+ 110 55) (0 0 110 0 55)]
    [honors-quiz "Honors Quiz" 5 (1 1 1 1 1)]))
    

(define name-format "Student: {username} Section: {Lab} Grader: <{Grader}>")
;; (define name-format "({Status}) {FormalLast} {FormalFirst}")

(define max-score 100)

(define smooth-range '(8 6))

(define graded-file "graded.rkt")
(define orig-file   "grading/text.rkt")
(define grade-file  "grade")
(define html-file   "graded.html")

;; ----------------------------------------------------------------------------

(require racket/list racket/match racket/file)

(provide name-format smooth-range graded-file orig-file grade-file html-file)

(define-struct conf (tag name tag-total weight) #:prefab)

(define hwname->conf
  (let ([t (make-hash)])
    (define (hwname->conf hwname)
      (define m (regexp-match #rx"^([a-zA-Z-]+)([0-9]+)?$" hwname))
      (define conf (and m (assq (string->symbol (cadr m)) grades-conf)))
      (define num (and m (and (caddr m) (string->number (caddr m)))))
      (define weights (and conf (last conf)))
      (and weights (<= 1 (or num 1) (length weights))
           (let ([w (list-ref weights (sub1 (or num 1)))])
             (make-conf (car conf)
                        (if num (format "~a #~a" (cadr conf) num) (cadr conf))
                        (caddr conf) w))))
    (λ (hwname) (hash-ref! t hwname (λ () (hwname->conf hwname))))))

(provide hw-to-grade? hw-title hw-max-score)
(define (hw-to-grade? hwname) (and (hwname->conf hwname) #t))
(define (hw-title     hwname) (conf-name (hwname->conf hwname)))
(define (hw-max-score hwname) (conf-weight (hwname->conf hwname)))

(provide grade-description)
(define (grade-description grade)
  (if (number? grade)
    #f
    (case grade
      [(- --) "no submission"]
      [(? ??) "not graded yet"]
      [(* **) "waived"]
      [(*-) "optional hw waived"]
      [else (error 'grade-description "bad grade value: ~s" grade)])))

;; returns a number or #f to ignore it
(define (grade->number grade)
  (if (number? grade)
    grade
    (case grade
      [(* ** *- ? ??) #f]
      [(- --) 0]
      [else (error 'grade->number "bad grade value: ~s" grade)])))

(provide student-single-grade student-total-grade additional-filter+sort)
(define (weighted grades+weights)
  (and (pair? grades+weights)
       (let ([grades  (map car grades+weights)]
             [weights (map cdr grades+weights)])
         (/ (apply + (map * grades weights)) (apply + weights)))))
(define tag-names   (map car grades-conf))
(define tag-weights (map caddr grades-conf))
(define (student-total-grade hws+grades student info)
  (define hws    (map car hws+grades))
  (define grades (map cdr hws+grades))
  (define confs  (map hwname->conf hws))
  (define tag-grades+weights
    (for/list ([tag tag-names] [tag-weight tag-weights])
      (define tag-grade
        (weighted (filter-map
                   (λ (conf grade)
                     (and (eq? tag (conf-tag conf))
                          (let ([grade (grade->number grade)])
                            (and grade (cons grade (conf-weight conf))))))
                   confs grades)))
      (and tag-grade (cons tag-grade tag-weight))))
  (weighted (filter values tag-grades+weights)))
(define master-homeworks
  (get-preference 'pl-course:master-homeworks (λ () '())))
(define honors-homeworks
  '("honors-exam1" "honors-hw3" "honors-quiz1" "honors-quiz2" "honors-hw5" "honors-quiz3" "honors-quiz4" "honors-quiz5"))
(define regular-homeworks
  '("hw3" "quiz1" "quiz2" "hw5" "quiz3" "quiz4"))
(define (student-single-grade hwname grade student info)
  (match grade
    [(list n m) (/ n m)]
    [(? number?) (/ grade max-score)]
    [(? symbol?) grade]
    [#f '-- 
       (cond 
         [(equal? "lab7" (info "Section")) 
           (if (member hwname regular-homeworks) '*- '--)] 
         [(not (equal? "lab7" (info "Section"))) 
           (if (member hwname honors-homeworks) '*- '--)])]
    #;[#f '-- (if (and (member hwname master-homeworks) (equal? "U" (info "Status")))
          '*- '--)]
    [_ (error 'student-single-grade
              "bad grade for ~s/~a: ~s" student hwname grade)]))

(define additional-filter+sort
  (let ([filtered
         (λ (name status)
           (λ (xs)
             (cons name (filter (λ (x) (equal? status (x "Status"))) xs))))]
        [global-key
         (λ (x) (x '(substs "{Status} {FormalLast} {FormalFirst}")))])
    (list (filtered "Undergrads" "U")
          (filtered "Masters" "M")
          (λ (xs) (cons "Sorted" (sort xs string<?
                                       #:key global-key #:cache-keys? #t))))))
