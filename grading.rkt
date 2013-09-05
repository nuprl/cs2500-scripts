#lang racket
;; TODO: clean up
(require file/tar
         net/smtp
         net/base64
         net/head
         openssl)
(provide send-assignments-to-graders
;         grader-assignment
;         grader
;         email-grader
         problem-set
         sanity-check-grades
         post-grades
         )

;; name (string) x dir (string)
(struct problem-set (name dir))
;; user (string) x name (string) x email (string)
(struct grader (user name email))
;; grader x (listof users)
(struct grader-assignment (grader ps users))
;; name (string) x dir (path)
(struct user (name dir))
;; grader x string x number
(struct grade-entry (grader username grade))

;; get-config
;; symbol -> any
;; retrieves the config value for the symbol s from config.rktd 
(define (get-config s)
  (let ([config-ls (with-input-from-file "config.rktd" read)])
    (or (second (assq s config-ls)))))

;; TODO: (define-parameters (name ...) (default ...) (guard ...))
;; string
(define course-name (make-parameter (get-config 'course-name)))
;; string
(define head-ta-email (make-parameter (get-config 'head-ta-email)))
;; string
(define head-ta-name (make-parameter (get-config 'head-ta-name)))
;; string
(define server-dir (make-parameter (get-config 'server-dir)))
;; string
(define smtp-server (make-parameter (get-config 'smtp-server)))
;; string
(define smtp-port (make-parameter (get-config 'smtp-port)))
;; string
(define smtp-user (make-parameter (get-config 'smtp-user)))
(define smtp-passwd (make-parameter (get-config 'smtp-passwd)))

;; (listof grader)
(define graders 
  (make-parameter 
    (map (lambda (triple) (grader (car (string-split (second triple) "@"))
                                  (car triple) (second triple)))
    (with-input-from-file "tutors.txt" read))))

;; send-assignments-to-graders
;; problem-set ... -> void
;; assigns graders in tutors.txt a rougly equal number of assignments
;; to grade from each problem-set. each grader is sent a seperate email
;; for each problem-set.

;; The email will be in the following format:
;;
;; To: <grader-email> 
;; From: <head-ta-email>
;; Subject: <course-name> Grading, <problem-set>
;; Body: <any>
;; Attachments: <problem-set>.tar.gz
;; 
;; <problem-set>.tar.gz will contain a folder named <username> for
;; each student. The folder will contain all material handed in for
;; <problem-set>
(define (send-assignments-to-graders . problem-sets)
  (let* ([gras (flatten (map assign-graders problem-sets))]
         [files (map tar-assignments gras)]) 
    (for-each email-grader gras files)
    (for-each delete-file files)))

;; TODO: Run local smtp server to test email functions
#;(module+ test
  (require rackunit)
  (parameterize ([server-dir "/tmp"]
                 [graders (list (grader "grader1" "grader1" "wilbowma@ccs.neu.edu")
                                (grader "grader2" "grader2" "wilbowma@ccs.neu.edu"))])
    (for-each (compose make-directory* (curry build-path "/tmp/test") symbol->string)
      '(test1 test2 test3 test4 test5 test6))
    (send-assignment-to-graders (problem-set "test" "test")))
  (delete-directory/files "/tmp/test"))

;; assign-graders
;; problem-set -> (listof grader-assignment)
;; assigns each grader an about equal  number of users based on the
;; number of turned-in assignments
(define (assign-graders ps) 
  (let* ([users (map (lambda (x) 
                       (call-with-values (thunk (split-path x)) 
                         (lambda (z path y)
                            (user (path->string path) x))))
                 (directory-list (build-path (server-dir) (problem-set-dir ps)) #:build? #t))]
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
    (for-each (compose make-directory* (curry build-path "/tmp/test") symbol->string)
      '(test1 test2 test3 test4 test5 test6))
    (check-equal? 
      (length (assign-graders (problem-set "Test" "test")))
      2)
    (check-equal?
      (length (grader-assignment-users (car (assign-graders (problem-set "Test" "test")))))
      3))
  (delete-directory/files "/tmp/test")
  #;(for-each (compose delete-directory (curry build-path "/tmp/test"))
            '(test1 test2 test3 test4 test5 test6)))

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
;; grader-assignment -> path to <problem-set>.tar.gz
;; given a grader-assignment, create
;; <problem-set>.tar.gz that contains a folder named <username-string>
;; for each student. This folder contains all material handed in for
;; <problem-set> in plain text.
(define (tar-assignments gra)
  (let ([file (format "/tmp/~a-~a.tar.gz"
                      (grader-user (grader-assignment-grader gra)) 
                      (problem-set-name (grader-assignment-ps gra)))])
       (apply (curry tar-gzip file)
         (map user-dir (grader-assignment-users gra)))
       file))

(module+ test
  (require rackunit)
  (parameterize ([server-dir "/tmp"]
                 [graders (list (grader "grader1" "grader1" "wilbowma@ccs.neu.edu")
                                (grader "grader2" "grader2" "wilbowma@ccs.neu.edu"))])
    (for-each (compose (lambda (dir) (make-directory* dir) (with-output-to-file (build-path dir "handin.rkt") (thunk (display 120)))) (curry build-path "/tmp/test") symbol->string)
      '(test1 test2 test3 test4 test5 test6))
    (let* ([ps (problem-set "Test" "test")]
           [files (map tar-assignments (assign-graders ps))])
      (map (lambda (file) (check-true (file-exists? file))) files)
      #;(map delete-file files))
  #;(for-each (compose delete-directory (curry build-path "/tmp/test"))
            '(test1 test2 test3 test4 test5 test6))
  (delete-directory/files "/tmp/test")))

;; email-grader
;; grader-assignment, path -> void
;; emails a grader their grading assignment, in the form of a .tar.gz containing their assignments
(define (email-grader gra ps.tar.gz)
  (let* ([to-addr (grader-email (grader-assignment-grader gra))]
         [ps (grader-assignment-ps gra)]
         [filename (call-with-values (thunk (split-path ps.tar.gz)) 
           (lambda (z path x) (path->string path)))]
        ;; TODO: This is awful -- fix net/smtp and net/sendmail to allow
        ;; attachments
        [bound "-q1w2e3r4t5"]
        [message 
          (list (format "Content-Type: application; name=\"~a\""
              filename)
            "Content-Transfer-Encoding: base64"
            (format "Content-Disposition: attachment; filename=\"~a\""
              filename)
            (with-output-to-string 
              (thunk (base64-encode-stream (open-input-file ps.tar.gz) 
                (current-output-port)))))])
    (smtp-send-message (smtp-server) 
                     (head-ta-email) 
                     (list to-addr)
                     (append-headers
                       (standard-message-header 
                      (head-ta-email) (list to-addr) (list) (list) 
                       (format "~a Grading, ~a" (course-name) (problem-set-name ps)))
            (append-headers
              "MIME-Version: 1.0"
              (format "Content-Type: multipart/mixed; boundary=\"~a\"\n" bound)))
                     message
                     #:tls-encode ports->ssl-ports
                     #:port-no (smtp-port)
                     #:auth-user (smtp-user)
                     #:auth-passwd (smtp-passwd))))

;; get-graded-problem-set
;; problem-set -> (listof grades)
;; retrieves graded problem sets from IMAP server by looking in (grade-mail-box) for emails matching the below format:
;;
;; To: (head-ta-email)
;; From: doesn't matter
;; Subject: (course-name) graded <problem-set-name>
;; Body: doesn't matter
;; Attachments: grades.rkt, graded-<problem-set-name>.tar.gz
(define (get-graded-problem-set ps) (void))

;; sanity-check-grades
;; problem-set, (listof grade-entry) -> report??
;; Checks that there are grades for each student that handed in an assignment, and reports anomolies in grades (>100%, <0%, students with low/decreasing averages)
(define (sanity-check-grades ps grades) (void))

;; post-grades
;; problem-set, (listof grade-entry) -> void
;; posts grades to the handin server
(define (post-grades ps grades) (void))
