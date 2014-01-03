#lang racket
(require
  racket/cmdline
  "cs2500-scripts/problem-set.rkt"
  "cs2500-scripts/grading.rkt"
  (only-in "cs2500-scripts/config.rkt" grade-server-dir))

(define-values (psn points) (command-line
  #:program "update-grades"
  #:args (problem-set-name points)
  (values problem-set-name (string->number points))))

(define ps (problem-set psn psn 0 0))

(let ([files (find-files (lambda (x) (regexp-match #rx"[^0-9]/grades.rkt" x))
                         ;; TODO: Should probably use build-path
                         (format "~a/~a" (grade-server-dir) psn))])
  (when (null? files)
     (error 'update-grades "Found no grades: ~a" psn))
  (for-each (compose (lambda (x) (grades->handin ps points x #:force #t))
                     (curryr with-input-from-file read)) files))
