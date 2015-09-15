#lang racket

(define postfix-special-tokens
  '(add div eq exec gt lt mul nget pop rem sel sub swap))

(define (postfix-special-token? x)
  (pair? (member x postfix-special-tokens)))

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
  (define (binop op)
    (let* ((v1 (car stack))
           (v2 (cadr stack))
           (new-stack (cons (op v2 v1) (cddr stack))))
      (values new-stack upcoming-cmds)))
  
  (define (relational-binop op)
    (binop (lambda (v2 v1)
             (if (op v2 v1) 1 0))))

  (define (handle-pop)
    (if (empty? stack)
        (error "failed to pop: stack already empty")
        (values (cdr stack) upcoming-cmds)))

  (define (handle-swap)
    (if (not (and (pair? stack)
                  (pair? (cdr stack))))
        (error "failed to swap: stack has fewer than two elements")
        (values (cons (cadr stack)
                      (cons (car stack)
                            (cddr stack)))
                upcoming-cmds)))

  (define (handle-sel)
    (if (not (and (pair? stack)
                  (pair? (cdr stack))
                  (pair? (cddr stack))))
        (error "failed to sel: stack has fewer than three elements")
        (let ((v1 (car stack))
              (v2 (cadr stack))
              (v3 (caddr stack)))
          (if (not (integer? v3))
              (error "failed to sel: 3rd element isn't an integer")
              (values (cons (if (= v3 0) v1 v2)
                            (cdddr stack))
                      upcoming-cmds)))))
  
  (define (handle-nget)
    (if (not (pair? stack))
        (error "failed to nget: stack is empty")
        (let ((v-index (car stack)))
          (if (not (and (integer? v-index)
                        (positive? v-index)
                        (<= v-index (length (cdr stack)))))
              (error "failed to nget: top element is not a valid index")
              (let ((v (list-ref stack v-index)))
                (if (not (integer? v))
                    (error "failed to nget: indexed value is not an integer")
                    (values (cons v (cdr stack))
                            upcoming-cmds)))))))

  (define (handle-exec)
    (let ((new-stack (cdr stack))
          (new-cmds (append (car stack) upcoming-cmds)))
      (values new-stack new-cmds)))
  
  (case token
    ['add (binop +)]
    ['div (binop quotient)] ; TODO: check semantics
    ['mul (binop *)]
    ['rem (binop remainder)] ; TODO: check semantics
    ['sub (binop -)]
    ['eq (relational-binop =)]
    ['gt (relational-binop >)]
    ['lt (relational-binop <)]
    ['pop (handle-pop)]
    ['swap (handle-swap)]
    ['sel (handle-sel)]
    ['nget (handle-nget)]
    ['exec (handle-exec)]))

(provide run-postfix)
