#! /usr/bin/env racket

#lang racket

(require "engine.rkt")

;; Allows eval to use identifiers in this namespace.
;; See http://stackoverflow.com/a/20783438
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(let* ((args (vector->list (current-command-line-arguments)))
       (postfix-file (car args))
       (initial-stack (map string->number (cdr args))))
  (unless (file-exists? postfix-file)
    (error "specified PostFix file does not exist"))
  (when (member #f initial-stack)
    (error "initial stack has an invalid integer"))
  
  (let ((postfix-src (read (open-input-file postfix-file))))
    (eval `(run-postfix ',postfix-src
                        ,@initial-stack)
          ns)))
