;;; gosh-config.el --- Gauche programming tool first load interface.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: lisp gauche scheme config
;; URL: https://github.com/mhayashi1120/Emacs-gosh-mode/raw/master/gosh-config.el
;; Emacs: GNU Emacs 22 or later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; 

;;; Code:

(require 'gosh-mode)

;; no error exists or not
(require 'auto-complete-config nil t)

(when (or (not (boundp 'scheme-program-name)) 
          (and scheme-program-name (not (string-match "gosh" scheme-program-name))))
  (setq scheme-program-name (format "%s -i" gosh-default-command)))



;; Most recent version is following
;; http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AEditingWithEmacs
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
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)

;;
;; Currently not listed.
;;

(put 'guard 'scheme-indent-function 1)
(put 'if 'scheme-indent-function 1)
(put 'if-let1 'scheme-indent-function 2)
(put 'let-keywords 'scheme-indent-function 2)
(put 'rlet1 'scheme-indent-function 2)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'without-echoing  'scheme-indent-function 1)

;; TODO incorrect at partical-scheme?
(put 'with-output-to-string 'scheme-indent-function 0)
(put 'ecase 'scheme-indent-function 1)



(add-to-list 'interpreter-mode-alist '("gosh" . gosh-mode))

;; font lock user customizable
(font-lock-add-keywords 
 'gosh-mode
 `(("\\`#.+" 0 font-lock-comment-delimiter-face)
   (gosh-font-lock-keywords 1 font-lock-keyword-face)
   (gosh-font-lock-basic-syntax 
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face nil t))))

(add-to-list 'auto-mode-alist 
             '("\\.scm\\(?:\\.[0-9]+\\)?$" . gosh-mode))

(add-to-list 'auto-mode-alist 
             '("\\.stub\\(?:\\.[0-9]+\\)?$" . scheme-mode))

(add-hook 'gosh-mode-hook 
          (lambda () 
            (make-local-variable 'lisp-indent-function)
            (setq lisp-indent-function 'gosh-smart-indent)
            (setq indent-tabs-mode nil)
            (gosh-sticky-mode-on)
            (turn-on-eldoc-mode)))

(add-hook 'gosh-inferior-mode-hook
          (lambda () 
            (turn-on-eldoc-mode)))

(when (featurep 'auto-highlight-symbol)
  (add-to-list 'ahs-modes 'gosh-mode))



(gosh-initialize)



(provide 'gosh-config)

;;; gosh-config.el ends here
