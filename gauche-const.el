;;; gauche-const.el --- Gauche constant settings



;;; Commentary:
;; 

;;; Code:

(defconst gauche-defined-procedure-keyword-list
  '( 
    ;; define methods
    "define" "define-class" "define-condition-type" "define-constant"
    "define-generic" "define-in-module" "define-macro" "define-method"
    "define-module" "define-record-type" "define-syntax" "define-values"

    ;; TODO
    "require" "provide"

    ;; keywords special indent function by `scheme-indent-function' property
    "and-let*" "begin0" "call-with-client-socket"
    "call-with-input-conversion" "call-with-input-file"
    "call-with-input-process" "call-with-input-string"
    "call-with-iterator" "call-with-output-conversion"
    "call-with-output-file" "call-with-output-string"
    "call-with-temporary-file" "call-with-values" "dolist" "dotimes"
    "guard" "if" "if-let1" "if-match" "let*-values" "let-args"
    "let-keywords*" "let-match" "let-optionals*" "let-syntax"
    "let-values" "let/cc" "let1" "letrec-syntax" "make" "match"
    "multiple-value-bind" "parameterize" "parse-options" "receive"
    "rlet1" "rxmatch-case" "rxmatch-cond" "rxmatch-if" "rxmatch-let"
    "syntax-rules" "unwind-protect" "unless" "until" "when" "while"
    "with-builder" "with-error-handler" "with-error-to-port"
    "with-input-conversion" "with-input-from-port"
    "with-input-from-process" "with-input-from-string" "with-iterator"
    "with-locking-mutex" "with-module" "with-output-conversion"
    "with-output-to-port" "with-output-to-process"
    "with-output-to-string" "with-port-locking" "with-signal-handlers"
    "with-string-io" "with-time-counter"
    ))

(defvar gauche-defined-procedure-keyword-regexp
  (regexp-opt gauche-defined-procedure-keyword-list))

(provide 'gauche-const)

;;; gauche-const.el ends here
