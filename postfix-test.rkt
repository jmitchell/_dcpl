#lang racket

(require rackunit
         "postfix.rkt")

(check-equal? (run-postfix '(postfix 4 lt (add) (mul) sel exec) 3 4 5 6) 30)
(check-equal? (run-postfix '(postfix 4 lt (add) (mul) sel exec) 4 3 5 6) 11)
