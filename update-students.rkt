(require "cs2500-scripts/students.rkt")

;; read current students
(define cur-students (read-students))

;; read partner/grader assignments
;; (list-of (list-of symbol symbol symbol symbol))
(define partners
  (apply append
         (map (lambda (x) (with-input-from-file x read))
              (build-list 3 (lambda (x) (format
                                          "/tmp/lab~a-partners.rkt"
                                          (add1 x)))))))

;; filter partner/grader assignemtns to student -> grader mapping
(define student->grader
  (let ([als (for/fold ([acc '()])
            ([tuple partners])
            (if (= 4 (length tuple))
                (list* (cons (first tuple) (fourth tuple))
                       (cons (second tuple) (fourth tuple))
                       acc)
                (list* (cons (first tuple) (third tuple)) acc)))])
       (lambda (username)
         (cond
           [(assoc username als) => cdr]
           [else #f]))))

(define (update-grader stu)
  (cond
    [(student->grader (student-username stu)) =>
       (lambda (grader)
          (student
            (student-username stu)
            (student-passwdhash stu)
            (student-id stu)
            (student-name stu)
            (student-email stu)
            (student-section stu)
            grader))]
    [else stu]))

(with-output-to-file "/tmp/new-users.rktd"
  (thunk (pretty-write (students->users.rktd (map update-grader cur-students))))
  #:exists 'replace)
