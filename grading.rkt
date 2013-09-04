#lang racket
(require file/tar
         net/smtp
         net/base64
         net/head
         openssl)
(provide ;assign-to-graders
         ;record-graded-assignment
         ;post-graded-problemset
         email-grader
         problem-set)

;; name (string) x dir (string)
(struct problem-set (name dir))
;; name (string) x email (string)
(struct grader (name email))
;; grader x (list of users)
(struct grader-assignment (grader users))
;; name (string) x dir (path)
(struct user (name dir))

;; (listof grader)
(define graders 
  (map (lambda (triple) (grader (car triple) (second triple)))
    (with-input-from-file "tutors.txt" read)))

;; get-config
;; symbol -> any
;; retrieves the config value for the symbol s from config.rktd 
(define (get-config s)
  (let ([config-ls (with-input-from-file "config.rktd" read)])
    (or (second (assq s config-ls)))))

;; string
(define course-name (get-config 'course-name))
;; string
(define head-ta-email (get-config 'head-ta-email))
;; string
(define head-ta-name (get-config 'head-ta-name))
;; string
(define server-dir (get-config 'server-dir))

;; email-graders
;; (listof problem-set) -> void
;; assigns graders in tutors.txt a rougly equal number of assignments
;; to grade from each problem-set. each grader is sent a seperate email
;; for each problem-set.

;; The email will be in the following format:
;;
;; To: <grader-email> 
;; From: <head-ta-email>
;; Subject: <course-name> Grading, <problem-set>
;; Body: empty
;; Attachments: <problem-set>.tar.gz
;; 
;; <problem-set>.tar.gz will contain a folder named <username-string> for
;; each student. The folder will contain all material handed in for
;; <problem-set>
(define (email-graders problem-set-ls) 
  (void)
  #;(for-each 
    (compose assign-graders tar-assignments email-assignments) 
    problem-set-ls))

;; assign-graders
;; problem-set -> (listof grader-assignment)
;; assigns each grader an about equal  number of users based on the
;; number of turned-in assignments
(define (assign-graders ps) 
  (let* ([users (map (lambda (x) 
                       (call-with-values (split-path x) 
                         (lambda (z path x)
                            (user (path->string path) path))))
                 (directory-list (build-path server-dir (problem-set-dir ps))))]
         [users (shuffle users)]
         [n (ceiling (/ (length users) (length graders)))])
    (map (lambda (grader users) (grader-assignment grader users))
         (shuffle graders)
         (split-into-chunks n users))))

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
;; problem-set, grader-assignment -> path to <problem-set>.tar.gz
;; given a problem set and a grader-assignment, create
;; <problem-set>.tar.gz that contains a folder named <username-string>
;; for each student. This folder contains all material handed in for
;; <problem-set> in plain text.
(define (tar-assignments ps gra)
  (apply (curry tar-gzip (format "/tmp/~a.tar.gz" (problem-set-name ps)))
         (map user-dir (grader-assignment-users gra))))

;; email-grader
;; grader-email, problem-set, path to <problem-set>.tar.gz -> void
;; emails a grader their grading assignment
(define (email-grader to-addr ps ps.tar.gz)
  (let* ([filename (call-with-values (thunk (split-path ps.tar.gz)) 
           (lambda (z path x) (path->string path)))]
        ;; TODO: This is awful -- fix net/smtp and net/sendmail to allow
        ;; attachments
        [bound "-q1w2e3r4t5"]
        [message 
          (list (format "--~a" bound)    
            (format "Content-Type: application; name=\"~a\""
              filename)
            (format "Content-Transfer-Encoding: base64")
            (format "Content-Disposition: attachment; filename=\"~a\""
              filename)
            (with-output-to-string 
              (thunk (base64-encode-stream (open-input-file ps.tar.gz) 
                (current-output-port)))))])
    (smtp-send-message (get-config 'smtp-server)
                     head-ta-email 
                     (list to-addr)
                     (append-headers
                       (standard-message-header 
                       head-ta-email (list to-addr) (list) (list) 
                       (format "~a Grading, ~a" course-name (problem-set-name ps)))
            (format "Content-Type: multipart/mixed; boundary=\"~a\"" bound))
                     message
                     #:tls-encode ports->ssl-ports
                     #:port-no (get-config 'smtp-port)
                     #:auth-user (get-config 'smtp-user)
                     #:auth-passwd (get-config 'smtp-passwd))))

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
