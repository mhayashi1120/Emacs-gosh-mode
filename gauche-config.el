;;; gauche-config.el --- Gauche programming tool first load interface.


;;; Commentary:
;; 

;;TODO
;; (setq scheme-program-name "gosh -i")
;; one command add exports


;;; Code:

(eval-when-compile
  (require 'cl))

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(when (or (not (boundp 'scheme-program-name)) 
	  (and scheme-program-name (not (string-match "gosh" scheme-program-name))))
  (setq scheme-program-name "gosh -i"))

;;TODO reconsider require or not.
;; (require 'quack nil t)

(require 'gauche-const)
(require 'gauche-refactor)
(require 'gauche-browse)
(require 'gauche-env)

(require 'scm-edit)



(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)
(put 'if 'scheme-indent-function 1)
(put 'if-let1 'scheme-indent-function 3)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if 'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)



(add-to-list 'interpreter-mode-alist '("gosh" . scheme-mode))

(font-lock-add-keywords
 'scheme-mode
 '(("\\`#.+" 0 font-lock-comment-delimiter-face)))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (make-local-variable 'eldoc-documentation-function)
	    (setq eldoc-documentation-function 'scm-eldoc-print-current-symbol-info)
	    (eldoc-mode)))

;;TODO fix key conventions
;; fix name space
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (define-key scheme-mode-map "\M-\C-i" 'scm-smart-complete)
	    (define-key scheme-mode-map "\C-cj" 'gauche-jump-to-module)
	    (define-key scheme-mode-map "\C-ch" 'gauche-jump-to-info)
	    (define-key scheme-mode-map "\C-cR" 'gauche-refactor-rename-symbol)
	    (define-key scheme-mode-map "\C-c\er" 'gauche-refactor-rename-symbol-afaiui)
	    ))



(provide 'gauche-config)

;;; gauche-config.el ends here
