#lang racket
(require "cs2500-scripts/problem-set.rkt"
         "cs2500-scripts/grading.rkt"
         "cs2500-scripts/config.rkt")

(write-problem-sets!
  (list (problem-set "hw2" "hw2") ;; active

  )
  (list (problem-set "test" "test") ;; inactive
        (problem-set "quiz1" "quiz1")
        (problem-set "quiz2" "quiz2")
        (problem-set "quiz3" "quiz3")
        (problem-set "quiz4" "quiz4")
        (problem-set "quiz5" "quiz5")
        (problem-set "quiz6" "quiz6")
        (problem-set "quiz7" "quiz7")
        (problem-set "honors-quiz1" "honors-quiz1")
        (problem-set "honors-quiz2" "honors-quiz2")
        (problem-set "honors-quiz3" "honors-quiz3")
        (problem-set "honors-quiz4" "honors-quiz4")
        (problem-set "honors-quiz5" "honors-quiz5")
        (problem-set "honors-quiz6" "honors-quiz6")
        (problem-set "honors-quiz7" "honors-quiz7")
        (problem-set "honors-quiz8" "honors-quiz8")
        (problem-set "exam1" "exam1")
        (problem-set "exam-two" "exam-two")
        (problem-set "honors-exam1" "honors-exam1")
        (problem-set "honors-exam-two" "honors-exam-two")
        (problem-set "hw2" "hw2")
        (problem-set "hw3" "hw3")
        (problem-set "honors-hw3" "honors-hw3")
        (problem-set "hw4" "hw4")
        (problem-set "hw5" "hw5")
        (problem-set "honors-hw5" "honors-hw5")
        (problem-set "hw6" "hw6")
        (problem-set "hw7" "hw7")
        (problem-set "hw8" "hw8")
        (problem-set "hw9" "hw9")
        (problem-set "honors-hw9" "honors-hw9")
        (problem-set "hw10" "hw10")
        (problem-set "hw11" "hw11")
        (problem-set "hw12" "hw12")

))

(send-assignments-to-graders (list (problem-set "hw2" "hw2")))
