#lang racket
(require "cs2500-scripts/problem-set.rkt"
         "cs2500-scripts/grading.rkt"
         "cs2500-scripts/config.rkt")
(send-assignments-to-graders (active-problem-sets (problem-sets)))
(write-current-problem-sets!)
