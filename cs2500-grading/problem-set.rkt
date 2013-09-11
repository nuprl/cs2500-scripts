#lang racket
;; TODO: test
;; Maybe this is too abstract and can be merged with gradebook...
(require "config.rkt")
(provide
  problem-set
  problem-set-name
  problem-set-dir)

;; name (string) x dir (string)
;; must be read-able and write-able
(struct problem-set (name dir) #:prefab)
