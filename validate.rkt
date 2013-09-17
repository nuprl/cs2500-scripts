#lang racket
(require net/smtp
         net/head
         openssl
         "cs2500-grading/config.rkt"
         "cs2500-grading/students.rkt")
(provide validate-users)

;; TODO: Abstract, move to cs2500-grading
;; TODO: rename cs2500-grading to cs2500-scripts

;; users = (listof (username (listof any/c)))
;; roster = (listof (username realname))
;; username = string
;; realname = string

;; validate-users
;; (listof students) roster -> (listof students) (listof students)
;; prints any usernames that do not appear in the roster
(define (validate-users students roster)
  (let* ([roster-users (map first roster)]
         [invalid-students (filter (lambda (student) (not (memq (student-username student) roster-users))) students)]
         [valid-students (filter (lambda (student) (memq (student-username student) roster-users)) students)])
    (values invalid-students valid-students)))

;; alert-invalid-users 
;; (listof users) -> void
;; sends an email to each of users alerting them to remake their account
(define (alert-invalid-users users)
  (for-each 
    (smtp-send-message
      (smtp-server)
      (head-ta-email)
      (list (head-ta-email))
      (standard-message-header 
        (head-ta-email) (list (head-ta-email)) (list) (list) "Invalid CS2500 Handin account")
      (list 
       (format "~a," (student-name x))
       ""
       "Your CS2500 handin account is invalid. Please reread http://www.ccs.neu.edu/course/cs2500f13/handin.html#(part._g1265) and create a new account."
       ""
       "If you believe your account was created correctly, please see a TA. It's possible you are not on the roster.")
      #:tls-encode ports->ssl-ports
      #:port-no (smtp-port)
      #:auth-user (smtp-user)
      #:auth-passwd (smtp-passwd))
    invalid))

(let-values ([(invalid valid) (validate-users students (append (with-input-from-file "whitelist-users.rktd" read) (with-input-from-file "roster.rkt" read)))])
  (with-output-to-file "users.rktd" (thunk (pretty-write (students->users.rktd valid))) #:exists 'replace)
  (with-output-to-file "invalid-users.rktd" (thunk (pretty-write (student->users.rktd invalid))) #:exists 'append)
  (alert-invalid-users invalid))