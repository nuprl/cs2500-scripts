#lang racket
(require "cs2500-grading/problem-set.rkt"
         "cs2500-grading/grading.rkt"
         "cs2500-grading/config.rkt")
(send-assignments-to-graders (active-problem-sets (problem-sets)))
(write-current-problem-sets!)
