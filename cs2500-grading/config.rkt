#lang racket
(provide (all-defined-out))

;; get-config
;; symbol -> any
;; retrieves the config value for the symbol s from config.rktd 
(define (get-config s)
  (let ([config-ls (with-input-from-file (build-path (current-directory) "config.rktd") read)])
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
;; grader-name, problem-set-name -> string
(define message-body (make-parameter (eval (get-config 'message-body)
                                           (make-base-namespace))))
;; string
(define grade-server-dir (make-parameter (get-config 'grade-server-dir)))

;; TODO: Make parameters
(define gradebook-path (build-path (current-directory) "gradebook.rktd"))
(define students-path (build-path (server-dir) "users.rktd"))
(define graders-path (build-path (current-directory) "graders.rktd"))
