;;; gosh-mode-make.el --- gosh-mode make lisp


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(setq ALL-MODULES 
      (list
       "gosh-config.el"
       "gosh-mode.el"
       "gosh-const.el"
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

(defun make-gosh-mode ()
  (gosh-mode-make-initialize))

(defun compile-gosh-mode ()
  (gosh-mode-make-initialize)
  (gosh-mode-make-compile))

(defun check-gosh-mode ()
  (gosh-mode-make-initialize)
  (gosh-mode-make-lint)
  (gosh-mode-make-compile)
  ;; see comment in `fsvn-test-excursion' at fsvn-test.el
  (condition-case err
      (progn
	(gosh-mode-make-test)
	(kill-emacs))
    (error
     (princ err)
     (kill-emacs 1))))

(defun install-gosh-mode ()
  (gosh-mode-make-initialize)
  (gosh-mode-make-install))

(defun uninstall-gosh-mode ()
  (gosh-mode-make-initialize)
  (gosh-mode-make-uninstall))

(defun what-where-gosh-mode ()
  (gosh-mode-make-initialize)
  (gosh-mode-make-install t))

(defun gosh-mode-make-initialize ()
  (let ((config (or (car command-line-args-left) "MAKE-CFG")))
    (setq load-path (cons "." load-path))
    (load config)))

(defun gosh-mode-make-compile ()
  (mapc
   (lambda (m)
     (byte-compile-file m))
   ALL-MODULES))

(defun gosh-mode-make-lint ()
  (elint-initialize)
  (mapc
   (lambda (module)
     (find-file module)
     (eval-buffer)
     (elint-current-buffer)
     (with-current-buffer "*Elint*"
       (message (replace-regexp-in-string "%" "%%" (buffer-string)))))
   ALL-MODULES))

(defun gosh-mode-make-uninstall (&optional just-print)
  (unless (file-directory-p INSTALL-DIR)
    (error "gosh-mode is not installed"))
  (gosh-mode-delete-directory INSTALL-DIR))


(defun gosh-mode-delete-directory (directory)
  (mapc
   (lambda (file)
     (cond
      ((member (file-name-nondirectory file) '("." "..")))
      ((file-symlink-p file)
       (error "Not supported"))
      ((file-directory-p file)
       (gosh-mode-delete-directory file))
      (t
       (princ (format "Deleting %s\n" file))
       (delete-file file))))
   (directory-files directory t "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
  (princ (format "Deleting %s\n" directory))
  (delete-directory directory))

(defun gosh-mode-make-install (&optional just-print)
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

(defun gosh-mode-make-test ()
  (mapc
   (lambda (m)
     (load-file m))
   ALL-MODULES)
  (load-file "gosh-mode-test.el")
  (princ "\n")
  (princ "-------------------------------------------------------------\n")
  (princ "Test completed\n")
  (princ "-------------------------------------------------------------\n")
  (princ "\n"))

;;; gosh-mode-make.el ends here
