#lang racket
;; TODO: clean up
;; TODO: Somethings are fragile with respect to paths, current directory
(require 
  file/zip
  net/smtp
  net/base64
  openssl
  net/head
  net/mime
  srfi/1
  "gradebook.rkt"
  "students.rkt"
  "graders.rkt"
  "problem-set.rkt"
  "config.rkt")
(provide
  send-assignments-to-graders
  ;grader-assignment
  ;grader
  ;email-grader
  sanity-check-grades
  post-grades)

;; grader x (listof group)
(struct grader-assignment (grader ps groups) #:prefab)
;; (listof user) x path
(struct group (users dir) #:prefab)

;; send-assignments-to-graders
;; (problem-set) -> void
;; assigns graders in tutors.txt a rougly equal number of assignments
;; to grade from each problem-set. each grader is sent a seperate email
;; for each problem-set.

;; The email will be in the following format:
;;
;; To: <grader-email> 
;; From: <head-ta-email>
;; Subject: <course-name> Grading, <problem-set>
;; Body: <any>
;; Attachments: <problem-set>.zip
;; 
;; <problem-set>.zip will contain a folder named <username> for
;; each student. The folder will contain all material handed in for
;; <problem-set>
(define (send-assignments-to-graders problem-sets)
  (let* ([gras (flatten (map assign-graders problem-sets))]
         [files (map tar-assignments gras)]) 
    (for-each email-grader gras files)
    (for-each delete-file files)))

;; TODO: Run local smtp server to test email functions
#;(module+ test
  (require rackunit)
  (parameterize ([server-dir "/tmp/"]
                 [graders (list (grader "grader1" "grader1" "wilbowma@ccs.neu.edu")
                                (grader "grader2" "grader2" "wilbowma@ccs.neu.edu"))])
    (for-each (compose (lambda (dir) 
                         (unless (directory-exists? dir) (make-directory* dir))
                         (with-output-to-file (build-path dir "handin.rkt")
                           (thunk (display 120))
                           #:exists 'truncate/replace)) 
                       (curry build-path "/tmp/test") symbol->string)
      '(test1 test2 test3 test4 test5 test6))
    (send-assignments-to-graders (list (problem-set "test" "test" 1234 1234))))
  (delete-directory/files "/tmp/test"))

;; assign-graders
;; problem-set -> (listof grader-assignment)
;; assigns each grader an about equal  number of users based on the
;; number of turned-in assignments
(define (assign-graders ps) 
  (let* ([users (map (lambda (x) 
                       (call-with-values (thunk (split-path x)) 
                         (lambda (z path y)
                            (group (string->students (path->string path)) 
                                   (build-path (problem-set-dir ps) path)))))
                 (directory-list (build-path (server-dir) (problem-set-dir ps))))]
         [users (shuffle users)]
         [graders (shuffle (graders))]
         [n (ceiling (/ (length users) (length graders)))])
    (map (lambda (grader users) (grader-assignment grader ps users))
         graders
         (split-into-chunks n users))))

(module+ test
  (require rackunit)
  (parameterize ([server-dir "/tmp"]
                 [graders (list (grader "grader1" "grader1" "wilbowma@ccs.neu.edu")
                                (grader "grader2" "grader2" "wilbowma@ccs.neu.edu"))])

    (for-each (compose (lambda (x) (unless (directory-exists? x)
                                     (make-directory* x))) 
                 (curry build-path "/tmp/test") 
                 symbol->string)
      '(test1 test2 test3 test4 test5 test6))
    (check-equal? 
      (length (assign-graders (problem-set "Test" "test" 1234 1234)))
      2)
    (check-equal?
      (length (grader-assignment-groups (car (assign-graders
                                               (problem-set "Test"
                                                            "test" 1234
                                                            1234)))))
      3))
  (delete-directory/files "/tmp/test"))

;; taken from http://stackoverflow.com/questions/8725832/how-to-split-list-into-evenly-sized-chunks-in-racket-scheme
(define (split-into-chunks n xs)
  (if (null? xs)
      '()
      (let ((first-chunk (take-up-to n xs))
            (rest (drop xs n)))
           (cons first-chunk (split-into-chunks n rest)))))

(define (take-up-to n xs)
  (if (or (zero? n) (null? xs))
      '()
      (cons (car xs) (take-up-to (- n 1) (cdr xs)))))


;; tar-assignments
;; grader-assignment -> path to <problem-set>.zip
;; given a grader-assignment, create
;; <problem-set>.zip that contains a folder named <username-string>
;; for each student. This folder contains all material handed in for
;; <problem-set> in plain text.
(define (tar-assignments gra)
  (let ([file (format "/tmp/~a-~a.zip"
                      (grader-user (grader-assignment-grader gra)) 
                      (problem-set-name (grader-assignment-ps gra)))])
       (parameterize ([current-directory (server-dir)])
         (apply (curry zip file)
                (map group-dir (grader-assignment-groups gra))))
       file))

(module+ test
  (require rackunit)
  (parameterize ([server-dir "/tmp/"]
                 [graders (list (grader "grader1" "grader1" "wilbowma@ccs.neu.edu")
                                (grader "grader2" "grader2" "wilbowma@ccs.neu.edu"))])
    (for-each (compose (lambda (dir) 
                         (unless (directory-exists? dir) (make-directory* dir))
                         (with-output-to-file (build-path dir "handin.rkt")
                           (thunk (display 120))
                           #:exists 'truncate/replace)) 
                       (curry build-path "/tmp/test") symbol->string)
      '(test1 test2 test3 test4 test5 test6))
    (let* ([ps (problem-set "Test" "test" 1234 1234)]
           [files (map tar-assignments (assign-graders ps))])
      (map (lambda (file) (check-true (file-exists? file))) files)
      (map delete-file files))
  #;(for-each (compose delete-directory (curry build-path "/tmp/test"))
            '(test1 test2 test3 test4 test5 test6))
  (delete-directory/files "/tmp/test")))

;; email-grader
;; grader-assignment, path -> void
;; emails a grader their grading assignment, in the form of a .tar.gz containing their assignments
(define (email-grader gra ps.tar.gz)
  (let* ([grader (grader-assignment-grader gra)]
         [to-addr (grader-email grader)]
         [ps (grader-assignment-ps gra)]
         [filename (call-with-values (thunk (split-path ps.tar.gz)) 
           (lambda (z path x) (path->string path)))]
        ;; TODO: This is awful -- fix net/smtp and net/sendmail to allow
        ;; attachments
        [bound "-q1w2e3r4t5"]
        [body ((message-body)
               (grader-name grader) 
               (grader-user grader)
               (grader-email grader)
               (problem-set-name ps))]
        [message 
          (list 
            "This is a message with multiple parts in the MIME format.\n"
            (format "--~a" bound)
            "Content-Type: text/plain; charset=us-ascii"
            "Content-Disposition: inline"
            "Content-Transfer-Encoding: 7bit\r\n"
            body
            (format "--~a" bound)
            (format "Content-Type: application/octet-stream; name=\"~a\""
              filename)
            "Content-Transfer-Encoding: base64"
            (format "Content-Disposition: attachment; filename=\"~a\"\r\n"
              filename)
            (with-output-to-string 
              (thunk (base64-encode-stream (open-input-file ps.tar.gz) 
                (current-output-port))))
            (format "--~a--" bound))])
    (smtp-send-message (smtp-server) 
                     (head-ta-email) 
                     (list to-addr (head-ta-email))
                     (append-headers 
                       (standard-message-header 
                           (head-ta-email) (list to-addr (head-ta-email)) (list) (list) 
                           (format "~a Grading, ~a" (course-name) (problem-set-name ps)))
                       (insert-field 
                         "MIME-Version"
                         "1.0"
                         (insert-field 
                           "Content-Type"
                           (format "multipart/mixed; boundary=\"~a\"" bound)
                           empty-header)))
                     message
                     #:tls-encode ports->ssl-ports
                     #:port-no (smtp-port)
                     #:auth-user (smtp-user)
                     #:auth-passwd (smtp-passwd))))

;; parse-graded-problem-set
;; path -> (listof grade-entry)
;; parses a graded-<problem-set-name>.tar.gz to a (listof grade-entry)
(define (parse-graded-problem-set path) (void))

;; sanity-check-grades
;; problem-set, (listof grade-entry) -> report??
;; Checks that there are grades for each student that handed in an assignment, and reports anomolies in grades (>100%, <0%, students with low/decreasing averages)
(define (sanity-check-grades ps grades) (void))

;; post-grades
;; problem-set, (listof grade-entry) -> void
;; posts grades to the handin server
(define (post-grades ps grades) (void))
