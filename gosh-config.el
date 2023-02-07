;;; gosh-config.el --- Gauche programming tool for quick configuration.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; URL: https://github.com/mhayashi1120/Emacs-gosh-mode

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

;; This module provides quick configurations for user. todo
;;

;;; Code:

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


(put 'and-let1 'scheme-indent-function 2)
(put 'quasirename 'scheme-indent-function 1)

;;
;; Currently not listed in wiliki.
;;

(put 'guard 'scheme-indent-function 1)
(put 'if 'scheme-indent-function 1)
(put 'if-let1 'scheme-indent-function 2)
(put 'let-keywords 'scheme-indent-function 2)
(put 'rlet1 'scheme-indent-function 2)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'without-echoing  'scheme-indent-function 1)
(put 'call-with-string-io 'scheme-indent-function 1)
(put 'with-ports 'scheme-indent-function 3)

;;
(put 'begin0 'scheme-indent-function 1)

;; TODO incorrect at partical-scheme?
(put 'with-output-to-string 'scheme-indent-function 0)
(put 'ecase 'scheme-indent-function 1)


;;;
;;; Mode definitions
;;;

;;
;; gosh mode
;;

(autoload 'gosh-mode "gosh-mode" nil t)
(autoload 'gosh-run "gosh-mode" nil t)

(add-to-list 'interpreter-mode-alist '("gosh" . gosh-mode))

(add-to-list 'auto-mode-alist '("\\.scm\\'" . gosh-mode))

(add-hook 'gosh-mode-hook
          (lambda ()
            (make-local-variable 'lisp-indent-function)
            (setq lisp-indent-function 'gosh-smart-indent)))

(add-hook 'gosh-mode-hook 'gosh-eval-mode-on)
(add-hook 'gosh-mode-hook 'turn-on-eldoc-mode)

(add-hook 'gosh-inferior-mode-hook 'turn-on-eldoc-mode)

;;
;; gosh stub mode
;;

(autoload 'gosh-stub-mode "gosh-stub" nil t)

(add-to-list 'auto-mode-alist '("\\.stub\\'" . gosh-stub-mode))

;;
;; Other useful mode
;;

(when (featurep 'auto-highlight-symbol)
  (add-to-list 'ahs-modes 'gosh-mode)
  (add-to-list 'ahs-modes 'gosh-stub-mode))


;;;
;;; configuration functions
;;;

;; define general indent rule
(defun gosh-config--define-general-indent ()
  (gosh-smart-indent-rule 'match 1 'util.match)
  (gosh-smart-indent-rule 'match-let 2 'util.match)
  (gosh-smart-indent-rule 'match-let* 1 'util.match)
  (gosh-smart-indent-rule 'match-letrec 1 'util.match)
  (gosh-smart-indent-rule 'match-let1 2 'util.match)
  (gosh-smart-indent-rule 'call-with-cgi-script 2 'www.cgi.test)
  (gosh-smart-indent-rule 'with-lock-file 1 'file.util)
  (gosh-smart-indent-rule 'let-values 1 'srfi-11)
  (gosh-smart-indent-rule 'let*-values 1 'srfi-11)
  (gosh-smart-indent-rule 'call-with-client-socket 1 'gauche.net)
  (gosh-smart-indent-rule 'call-with-input-conversion 1 'gauche.charconv)

  (gosh-smart-indent-rule '$let 1 'parser.peg)
  (gosh-smart-indent-rule '$let* 1 'parser.peg)

  (gosh-smart-indent-rule '^ 1)
  (gosh-smart-indent-rule 'glet 1 'gauche.generator)
  (gosh-smart-indent-rule 'glet* 1 'gauche.generator)
  (gosh-smart-indent-rule 'glet1 2 'gauche.generator)
  (gosh-smart-indent-rule 'do-generator 1 'gauche.generator)
  nil)

;; entry point of this configurations
(defun gosh-config-loading ()
  (gosh-initialize)
  (gosh-config--define-general-indent))

;; add hook after load substance of major-mode
(eval-after-load 'gosh-mode
  `(gosh-config-loading))

;; amend after load `gosh-mode'
(when (featurep 'gosh-mode)
  (gosh-config-loading))

;; for package (ELPA)
;;;###autoload (require 'gosh-config)

(defun gosh-config-unload-function ()
  (let (pair)
    (dolist (mode '(gosh-mode gosh-stub-mode))
      (while (setq pair (rassq mode auto-mode-alist))
        (setq auto-mode-alist
              (delq pair auto-mode-alist))))
    (dolist (mode '(gosh-mode))
      (while (setq pair (rassq mode interpreter-mode-alist))
        (setq interpreter-mode-alist
              (delq pair interpreter-mode-alist))))))

(provide 'gosh-config)

;;; gosh-config.el ends here
