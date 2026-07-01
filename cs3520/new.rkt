#lang plait

(equal? `{+ 1 2} `{+ 1 2})


(define (is-plus-numbers? se)
  (s-exp-match? `{+ NUMBER NUMBER} se))
