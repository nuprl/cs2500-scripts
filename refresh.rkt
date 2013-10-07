#lang racket
(require "cs2500-scripts/problem-set.rkt"
         "cs2500-scripts/grading.rkt"
         "cs2500-scripts/config.rkt")
(send-assignments-to-graders (list (problem-set "hw3" "hw3" 0 0)))
#;(write-problem-sets! 
  '()
  #;(list (problem-set "hw5" "hw5" 0 0) 
        (problem-set "honors-hw5" "honors-hw5" 0 0))
  (list (problem-set "hw1" "hw1" 0 0)
        (problem-set "hw2" "hw2" 0 0)
        (problem-set "hw3" "hw3" 0 0)
        (problem-set "honors-hw3" "honors-hw3" 0 0)
        (problem-set "hw4" "hw4" 0 0) 
        (problem-set "honors-hw4" "honors-hw4" 0 0)))

