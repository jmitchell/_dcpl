(require 'generic-x)

(define-generic-mode
    'dcpl-postfix
  '()                                   ; no comment support
  '("postfix"                           ; built-in keywords
    "pop"
    "swap"
    "sel"
    "nget"
    "exec"
    "add"
    "div"
    "mul"
    "rem"
    "sub"
    "eq"
    "gt"
    "lt")
  '("\\.pf$")                           ; *.pf files
  nil
  "PostFix mode (DCPL flavor)")
