#lang racket

(require rackunit
         "postfix-with-dup.rkt")

(check-equal? (run-postfix-with-dup '(postfix 0 3 dup add))
              6)
(check-equal? (run-postfix-with-dup '(postfix 1 (dup mul) exec)
                                    12)
              144)
(check-equal? (run-postfix-with-dup '(postfix 2 (dup mul) dup 3 nget swap exec swap 4 nget swap exec add)
                                    5 12)
              169)