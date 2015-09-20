#lang racket

(require "postfix.rkt")

(def-token 'dup
  (lambda (upcoming-commands stack)
    (values (cons (car stack) stack)
            upcoming-commands)))

(define run-postfix-with-dup run-postfix)

(provide run-postfix-with-dup)
