#lang racket/base

(require racket/list racket/string racket/runtime-path
         handin-server/private/config)

(provide remove-duplicates
         (all-from-out handin-server/private/config))

(provide course-dir course-file)
(define-runtime-path heredir ".")
(define basedir
  (or (getenv "COURSE")
      ;; (regexp-replace
      ;;  #rx"/$"
      ;;  (path->string (simplify-path (build-path heredir 'up)))
      ;;  "")
      (error 'course-path "expecting a COURSE environment variable")
      ))
(define ((course-path mode) . subpaths)
  (let* ([path (apply string-append basedir
                      (map (λ (sub) (string-append "/" sub)) subpaths))]
         [path (regexp-replace* #rx"//+" path "/")]
         [path (regexp-replace #rx"/+$" path "")])
    (unless ((case mode
               [(directory) directory-exists?]
               [(file)      file-exists?]
               [else (error 'course-path "bad mode: ~s" mode)])
             path)
      (error 'course-path "~a not found: ~a" mode path))
    path))
(define course-dir  (course-path 'directory))
(define course-file (course-path 'file))

(unless (getenv "PLT_HANDINSERVER_DIR")
  (void (putenv "PLT_HANDINSERVER_DIR" (course-dir "cs2500-f13-server"))))

;; expecting an optional predicate (for filtering) and a directory in any order
(provide ls)
(define (ls [dir #f] [pred? #f])
  (when (and dir (not pred?) (procedure? dir)) (set! pred? dir) (set! dir #f))
  (parameterize ([current-directory (or dir (current-directory))])
    (let ([l (sort (map path->string (directory-list)) string<?)])
      (if pred? (filter pred? l) l))))

(define (read-data-file f)
  (with-input-from-file (course-file "cs2500-f13-server" f) read))

;; ----------------------------------------------------------------------------

(provide user-info)
(define user-info
  (let ([table (make-hasheq)] [all '()])
    (define extra-fields (map car (get-conf 'extra-fields)))
    (for ([x (read-data-file "users.rktd")])
      (define fields
        (cons (cons "username" (symbol->string (car x)))
              (map cons (cons "password-hash" extra-fields) (cadr x))))
      (let ([status (cond [(assoc "Status" fields) => cdr] [else #f])])
        (unless (equal? status "???") ; use "S" tu remove staff
          (set! all (cons (car x) all))
          (hash-set! table (car x) fields))))
    (set! all (sort all string<? #:key symbol->string #:cache-keys? #t))
    (λ (student . fields)
      (if (eq? student 'ALL) ; ALL => get all students
        all
        (let ([info
               (hash-ref
                table (if (string? student) (string->symbol student) student)
                (λ () (error 'user-info "no record for ~s" student)))])
          (if (null? fields)
            info
            (apply values
                   (map (λ (f)
                          (cond [(assoc f info) => cdr]
                                [else (error 'user-info
                                             "no `~a' field in config.rktd"
                                             f)]))
                        fields))))))))

(define (subst str substs)
  (if (list? str)
    (map (λ (x) (subst x substs)) str)
    (let* ([m (regexp-match-positions #rx"{([^{}]+)}" str)]
           [s (and m (substring str (caadr m) (cdadr m)))])
      (if m
        (subst (string-append
                (substring str 0 (caar m))
                (cond [(assoc s substs) => (compose (lambda (x) (if (symbol? x) (symbol->string x) x)) cdr)]
                      [else (error 'subst "unknown substitution: ~s" s)])
                (substring str (cdar m)))
               substs)
        str))))

(provide user-substs)
(define (user-substs user str)
  (subst str (user-info user)))

;; ----------------------------------------------------------------------------

(provide print-table filler)

(define-struct filler (str) #:omit-define-syntaxes)

(define (filler x)
  (let ([str (if (string? x) x (format "~a" x))])
    (if (equal? str "")
      (error 'filler "empty filler: ~e" x)
      (make-filler str))))

(define (format-filler str width)
  (let* ([n (ceiling (/ width (string-length str)))]
         [str (if (= n 1) str
                  (string-append* (for/list ([i (in-range n)]) str)))])
    (if (= width (string-length str)) str (substring str 0 width))))

(define (format-string str width alignment)
  (let ([len (string-length str)])
    (if (>= len width) str
        (let ([pad (make-string (- width len) #\space)])
          (if (eq? alignment 'l)
            (string-append str pad)
            (string-append pad str))))))

(define (print-table table alignments [convert #f])
  (define table*
    (for/list ([row table])
      (for/list ([x row])
        (if (filler? x) x
            (let ([x (if convert (convert x) x)])
              (if (string? x) x (format "~a" x)))))))
  (define (col-width . col)
    (foldl (λ (x acc) (if (filler? x) acc (max acc (string-length x))))
           0 col))
  (define widths (apply map col-width table*))
  (for ([row table*])
    (let* ([line (map (λ (x width alignment)
                        (if (filler? x)
                          (format-filler (filler-str x) width)
                          (format-string x width alignment)))
                      row widths alignments)]
           [line (string-append* (add-between line " "))]
           [line (regexp-replace #rx" +$" line "")])
      (display line)
      (newline))))

;; ----------------------------------------------------------------------------
;; Experiment (see ~/work/tmp/syntax-for.rkt)

(require (for-syntax racket/base))
(provide syntax-for (for-syntax (all-from-out racket/base)))
(define-syntax-rule (syntax-for [p s] (combine E _))
  (begin (define-syntax (m stx)
           (define (loop r)
             (cond [(list? r) r]
                   [(not (syntax? r)) (error 'syntax-for "bad input")]
                   [(identifier? r) (loop (syntax-local-value r))]
                   [(syntax->list r)]
                   [else (error 'syntax-for "bad input")]))
           #`(combine
              #,@(for/list ([stx (map syntax-local-introduce (loop s))])
                   (with-syntax ([p stx]) #'E))))
         (m)))
