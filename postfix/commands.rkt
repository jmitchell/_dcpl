#lang racket

(define special-token-dictionary (make-hash))

(define (def-token token handler)
  ;; TODO: prevent overriding existing tokens
  (hash-set! special-token-dictionary token handler))

(def-token 'pop
  (lambda (upcoming-cmds stack)
    (if (empty? stack)
        (error "failed to pop: stack already empty")
        (values (cdr stack) upcoming-cmds))))

(def-token 'swap
  (lambda (upcoming-cmds stack)
    (if (not (and (pair? stack)
                  (pair? (cdr stack))))
        (error "failed to swap: stack has fewer than two elements")
        (values (cons (cadr stack)
                      (cons (car stack)
                            (cddr stack)))
                upcoming-cmds))))

(def-token 'sel
  (lambda (upcoming-cmds stack)
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
                      upcoming-cmds))))))

(def-token 'nget
  (lambda (upcoming-cmds stack)
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
                            upcoming-cmds))))))))

(def-token 'exec
  (lambda (upcoming-cmds stack)
    (let ((new-stack (cdr stack))
          (new-cmds (append (car stack) upcoming-cmds)))
      (values new-stack new-cmds))))

(define (def-binop-token token op)
  (def-token token
    (lambda (upcoming-cmds stack)
      (let* ((v1 (car stack))
             (v2 (cadr stack))
             (new-stack (cons (op v2 v1) (cddr stack))))
        (values new-stack upcoming-cmds)))))

(def-binop-token 'add +)
(def-binop-token 'div quotient)
(def-binop-token 'mul *)
(def-binop-token 'rem remainder)
(def-binop-token 'sub -)

(define (def-relational-token token op)
  (def-binop-token token
    (lambda (v2 v1)
      (if (op v2 v1) 1 0))))

(def-relational-token 'eq =)
(def-relational-token 'gt >)
(def-relational-token 'lt <)

;; TODO: reintegrate dup and its tests as an optional plugin.
;(def-token 'dup
;  (lambda (upcoming-commands stack)
;    (values (cons (car stack) stack)
;            upcoming-commands)))
;
;(check-equal? (run-postfix '(postfix 0 3 dup add))
;              6)
;(check-equal? (run-postfix '(postfix 1 (dup mul) exec)
;                           12)
;              144)
;(check-equal? (run-postfix '(postfix 2 (dup mul) dup 3 nget swap exec swap 4 nget swap exec add)
;                           5 12)
;              169)

(provide special-token-dictionary)