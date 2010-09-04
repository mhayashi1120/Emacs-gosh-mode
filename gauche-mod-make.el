;;; gauche-mod-make.el --- Gauche-mod make lisp


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(setq ALL-MODULES 
      (list
       "gauche-browse.el"
       "gauche-config.el"
       "gauche-const.el"
       "gauche-env.el"
       "gauche-refactor.el"
       "refactor.el"
       "scm-browse.el"
       "scm-const.el"
       "scm-edit.el"
       "scm-env.el"
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

(defun make-gauche-mod ()
  (gauche-mod-make-initialize))

(defun compile-gauche-mod ()
  (gauche-mod-make-initialize)
  (gauche-mod-make-compile))

(defun check-gauche-mod ()
  (gauche-mod-make-initialize)
  (gauche-mod-make-lint)
  (gauche-mod-make-compile)
  ;; see comment in `fsvn-test-excursion' at fsvn-test.el
  (condition-case err
      (progn
	(gauche-mod-make-test)
	(kill-emacs))
    (error
     (princ err)
     (kill-emacs 1))))

(defun install-gauche-mod ()
  (gauche-mod-make-initialize)
  (gauche-mod-make-install))

(defun what-where-gauche-mod ()
  (gauche-mod-make-initialize)
  (gauche-mod-make-install t))

(defun gauche-mod-make-initialize ()
  (let ((config (or (car command-line-args-left) "MAKE-CFG")))
    (setq load-path (cons "." load-path))
    (load config)))

(defun gauche-mod-make-compile ()
  (mapc
   (lambda (m)
     (byte-compile-file m))
   ALL-MODULES))

(defun gauche-mod-make-lint ()
  (elint-initialize)
  (mapc
   (lambda (module)
     (find-file module)
     (eval-buffer)
     (elint-current-buffer)
     (with-current-buffer "*Elint*"
       (message (replace-regexp-in-string "%" "%%" (buffer-string)))))
   ALL-MODULES))

(defun gauche-mod-make-install (&optional just-print)
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

(defun gauche-mod-make-test ()
  (mapc
   (lambda (m)
     (load-file m))
   ALL-MODULES)
  (load-file "gauche-mod-test.el")
  (princ "\n")
  (princ "-------------------------------------------------------------\n")
  (princ "Test completed\n")
  (princ "-------------------------------------------------------------\n")
  (princ "\n"))

;;; gauche-mod-make.el ends here
