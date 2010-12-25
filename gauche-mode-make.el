;;; gauche-mode-make.el --- gauche-mode make lisp


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(setq ALL-MODULES 
      (list
       "gauche-mode.el"
       "gauche-config.el"
       "gauche-const.el"
       "gauche-refactor.el"
       "refactor.el"
       ))

;; (when (memq system-type '(windows-nt))
;;   (setq ALL-MODULES
;; 	(append ALL-MODULES (list "fsvn-win.el")))
;;   (unless (featurep 'meadow)
;;     (setq ALL-MODULES
;; 	(append ALL-MODULES (list 
;; 			     "mw32cmp.el"
;; 			     "mw32script.el"
;; 			     )))))

(defun make-gauche-mode ()
  (gauche-mode-make-initialize))

(defun compile-gauche-mode ()
  (gauche-mode-make-initialize)
  (gauche-mode-make-compile))

(defun check-gauche-mode ()
  (gauche-mode-make-initialize)
  (gauche-mode-make-lint)
  (gauche-mode-make-compile)
  ;; see comment in `fsvn-test-excursion' at fsvn-test.el
  (condition-case err
      (progn
	(gauche-mode-make-test)
	(kill-emacs))
    (error
     (princ err)
     (kill-emacs 1))))

(defun install-gauche-mode ()
  (gauche-mode-make-initialize)
  (gauche-mode-make-install))

(defun what-where-gauche-mode ()
  (gauche-mode-make-initialize)
  (gauche-mode-make-install t))

(defun gauche-mode-make-initialize ()
  (let ((config (or (car command-line-args-left) "MAKE-CFG")))
    (setq load-path (cons "." load-path))
    (load config)))

(defun gauche-mode-make-compile ()
  (mapc
   (lambda (m)
     (byte-compile-file m))
   ALL-MODULES))

(defun gauche-mode-make-lint ()
  (elint-initialize)
  (mapc
   (lambda (module)
     (find-file module)
     (eval-buffer)
     (elint-current-buffer)
     (with-current-buffer "*Elint*"
       (message (replace-regexp-in-string "%" "%%" (buffer-string)))))
   ALL-MODULES))

(defun gauche-mode-make-install (&optional just-print)
  (unless (or just-print (file-directory-p INSTALL-DIR))
    (make-directory INSTALL-DIR t))
  (let (src dest elc el)
    (mapc
     (lambda (m)
       (setq el m)
       (setq elc (concat m "c"))
       (setq dest-el (expand-file-name el INSTALL-DIR))
       (setq dest-elc (expand-file-name elc INSTALL-DIR))
       (princ (format "%s -> %s\n" el dest-el))
       (princ (format "%s -> %s\n" elc dest-elc))
       (unless just-print
	 (mapc
	  (lambda (src-dest)
	    (let ((src (car src-dest))
		  (dest (cdr src-dest)))
	      (unless (file-exists-p src)
		(error "%s not exists." src))
	      (copy-file src dest t)
	      (set-file-modes dest ?\644)))
	  (list (cons el dest-el) (cons elc dest-elc)))))
     ALL-MODULES)))

(defun gauche-mode-make-test ()
  (mapc
   (lambda (m)
     (load-file m))
   ALL-MODULES)
  (load-file "gauche-mode-test.el")
  (princ "\n")
  (princ "-------------------------------------------------------------\n")
  (princ "Test completed\n")
  (princ "-------------------------------------------------------------\n")
  (princ "\n"))

;;; gauche-mode-make.el ends here
