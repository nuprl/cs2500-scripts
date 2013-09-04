#lang racket
(require file/tar
         net/smtp
         net/base64
         net/head
         openssl)
(provide ;assign-to-graders
         ;record-graded-assignment
         ;post-graded-problemset
         send-assignment-to-graders
         grader-assignment
         grader
         email-grader
         problem-set)

;; name (string) x dir (string)
(struct problem-set (name dir))
;; user (string) x name (string) x email (string)
(struct grader (user name email))
;; grader x (list of users)
(struct grader-assignment (grader ps users))
;; name (string) x dir (path)
(struct user (name dir))

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

;; send-assignment-to-graders
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
(define (send-assignment-to-graders . problem-sets)
  (let ([gras (flatten (map assign-graders problem-sets))]) 
    (for-each email-grader gras (map tar-assignments gras))))

;; assign-graders
;; problem-set -> (listof grader-assignment)
;; assigns each grader an about equal  number of users based on the
;; number of turned-in assignments
(define (assign-graders ps) 
  (let* ([users (map (lambda (x) 
                       (call-with-values (split-path x) 
                         (lambda (z path x)
                            (user (path->string path) path))))
                 (directory-list (build-path (server-dir) (problem-set-dir ps))))]
         [users (shuffle users)]
         [n (ceiling (/ (length users) (length graders)))])
    (map (lambda (grader users) (grader-assignment grader ps users))
         (shuffle graders)
         (split-into-chunks n users))))

(module+ test
  (require rackunit)
  (parameterize ([server-dir "/tmp"]
                 [graders (list (grader "grader1" "grader@ccs.neu.edu")
                                (grader "grader2" "grader@ccs.neu.edu"))])
    (make-directory "/tmp/test")
    (for-each (compose (curry build-path "/tmp/test") make-directory)
      '(test1 test2 test3 test4 test5 test6))
    (check-equal? 
      (length (assign-graders (problem-set "Test" "test")))
      2)
    (check-equal?
      (length (car (assign-graders (problem-set "Test" "test"))))
      3))
  (delete-directory "/tmp/test")
  (for-each (compose (curry build-path "/tmp/test") delete-directory)
            '(test1 test2 test3 test4 test5 test6)))

;; taken from http://stackoverflow.com/questions/8725832/how-to-split-list-into-evenly-sized-chunks-in-racket-scheme
(define (split-into-chunks n xs)
  (if (null? xs)
      '()
      (let ((first-chunk (take-up-to n xs))
            (rest (drop n xs)))
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
  (apply (curry tar-gzip 
           (format "/tmp/~a-~a.tar.gz"
              (grader-user (grader-assignment-grader gra)) 
              (grader-assignment-ps gra)))
    (map user-dir (grader-assignment-users gra))))

(module+ test
  (require rackunit)
  (parameterize ([server-dir "/tmp"]
                 [graders (list (grader "grader1" "grader@ccs.neu.edu")
                                (grader "grader2" "grader@ccs.neu.edu"))])
    (make-directory "/tmp/test")
    (for-each (compose (curry build-path "/tmp/test") make-directory)
      '(test1 test2 test3 test4 test5 test6))
    (let* ([ps (problem-set "Test" "test")]
          [file (tar-assignments ps (assign-graders ps))])
      (check-true (file-exists? file))
      (delete-file file)))
  (delete-directory "/tmp/test")
  (for-each (compose (curry build-path "/tmp/test") delete-directory)
            '(test1 test2 test3 test4 test5 test6)))

;; email-grader
;; grader-assignment, path to <problem-set>.tar.gz -> void
;; emails a grader their grading assignment
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

;; record-graded-assignment
;; Email -> void
;; parses an email in the below format, recording the grades from each
;; graded assignment into <problem-set>.rktd

;; To: wilbowma@ccs.neu.edu
;; From: <grader email@ccs.neu.edu>
;; Subject: Graded <problem-set> for <username-string>: <grade>
;; Body: empty
;; Attachments: <Gradedproblem-set>.tar.gz

;; <Gradedproblem-set>.tar.gz contains a folder named <username-string>
;; for each student. The folder contains all material handed in for
;; <problem-set>, annotated by the graders ;; Each annotated .rkt file
;; should start with the following strings
;; ;; <problem-set> Grade: <grade>
;; ;; Grader: <grader name> <grader email>
(define (record-graded-assignment email) (void))

;; post-graded-problemset
;; <problem-set> -> void
;; posts grades from <problem-set>.rktd to the handin server
(define (post-graded-problemset problem-set) (void))
#|
            "Content-Type: text/plain"
            "Content-Disposition: inline"
            (format "Hello grader,\nAttached is your grading assignment for the week. When you're done grading each pair's assignment, please send me an email, with a .tar.gz file named after the pair's username, containing all the annotated files from the problem set. Please format the email as follows:\n\n

To: wilbowma@ccs.neu.edu\n
From: <your .ccs or .husky email>\n
Subject: Graded <problem-set> for <username-string>: <grade>\n
Body: comments\n
Attachments: <username-string>.tar.gz\n

For example, suppose student1 and student2 are working together, and you just finished grading their assignment for problem set 2. Suppose they earned 8 out of 10 points. You should send me an email that looks like:

To: wilbowma@ccs.neu.edu\n
From: <your .ccs or .husky email>\n
Subject: Graded ps2 for student1+student2: 8/10\n
Body: Student1 is a pro. Student2 clearly needs to learn to write recursive functions better.\n
Attachments: student1+student2.tar.gz\n

student1+student2.tar.gz should contain all racket files student1+student2 turned in, with your annotations.")
|#
