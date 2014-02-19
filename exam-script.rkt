(define exam (with-input-from-file "exam.rkt" read))

(define (sum ls) (apply + ls))
(define (average ls) (/ (sum ls) (length ls)))
(define (median ls)
  (let ([len (length ls)]
        [ls (sort ls <)])
    (if (odd? len)
        (list-ref ls (ceiling (/ len 2)))
        (average (list (list-ref ls (/ len 2)) (list-ref ls (add1 (/ len 2))))))))

(define (to-pct x) (real->decimal-string (* 100.0 x) 2))

(let ([exam (map second exam)])
  (printf "Number of Exams: ~a\n" (length exam))
  (printf "Mean: ~a\n" (to-pct (average exam)))
  (printf "Median: ~a\n" (to-pct (median exam)))
  (printf "\nExams: ~a\n" (map to-pct (sort exam <))))
