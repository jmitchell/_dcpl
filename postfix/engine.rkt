#lang racket

(require "commands.rkt")

(define (postfix-special-token? x)
  (hash-has-key? special-token-dictionary x))

(define (postfix-executable-sequence? x)
  (and (list? x)
       (empty? (filter-not postfix-command? x))))

(define (postfix-command? x)
  (or (integer? x)
      (postfix-special-token? x)
      (postfix-executable-sequence? x)))

(define (postfix-program? x)
  (let ((postfix (car x))
        (argc (cadr x))
        (commands (cddr x)))
    (and
     (eq? postfix 'postfix)
     (integer? argc)
     (not (negative? argc))
     (postfix-command? commands))))

; TODO: whenever values are cons-ed onto the stack ensure type invariant holds
(define (postfix-stack-value? x)
  (or (integer? x)
      (postfix-executable-sequence? x)))

(define (postfix-eval commands stack)
  (cond
    [(empty? commands)
     (handle-final-stack stack)]
    [(postfix-command? (car commands))     
     (handle-command (car commands) (cdr commands) stack)]
    [else
     (error "invalid command")]))

(define run-postfix
  (lambda (program . stack)
    (if (not (postfix-program? program))
        (error "invalid program")
        (let ((argc (cadr program))
              (commands (cddr program)))
          (if (not (= argc (length stack)))
              (error "not enough input arguments on the stack")
              (postfix-eval commands stack))))))

(define (handle-final-stack stack)
  (if (not (and (pair? stack)
                (integer? (car stack))))
      (error "invalid final stack")
      (car stack)))

(define (handle-command x upcoming-cmds stack)
  (cond
    [(or (integer? x)
         (postfix-executable-sequence? x))
     (postfix-eval upcoming-cmds (cons x stack))]
    [(postfix-special-token? x)
     (define-values
       (new-stack new-upcoming-cmds) (handle-special-token x upcoming-cmds stack))
     (postfix-eval new-upcoming-cmds new-stack)]
    [else
     (error "invalid command")]))

(define (handle-special-token token upcoming-cmds stack)
  (let ((handler (hash-ref special-token-dictionary token)))
    (handler upcoming-cmds stack)))

(provide run-postfix)
