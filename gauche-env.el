;;; gauche-env.el --- Programming language gauche basic utilities


;;; Commentary:
;; 

(eval-when-compile
  (require 'cl))

(require 'scm-env)

;;; Code:

(defvar gauche-repo-path
  (or (car (remove-if-not #'file-directory-p
                          '("/usr/share/gauche"
                            "/usr/local/share/gauche"
                            "/opt/share/gauche"
                            "/opt/local/share/gauche")))
      (and (fboundp 'shell-command-to-string)
           (let* ((res (shell-command-to-string "gauche-config --syslibdir"))
                  (res (substring res 0 (- (length res) 1))))
             (and res (file-directory-p res)
                  (let* ((dir (file-name-directory res))
                         (dir2 (file-name-directory
                                (substring dir 0 (- (length dir) 1)))))
                    (substring dir2 0 (- (length dir2) 1))))))
      "/usr/local/share/gauche"))

(defvar gauche-site-repo-path
  (concat gauche-repo-path "/site/lib"))

(defun gauche-available-modules (&optional sym)
  (let ((version-dir
         (concat
	  ;;TODO version specific?
          (car (directory-files gauche-repo-path t "^[0-9]"))
          "/lib"))
        (site-dir gauche-site-repo-path)
        (other-dirs
         (remove-if-not
          #'(lambda (d) (and (not (equal d "")) (file-directory-p d)))
          (gauche-environ-load-path))))
    (setq other-dirs
          (mapcar 'directory-file-name other-dirs))
    (mapcar
     #'(lambda (f) (subst-char-in-string ?/ ?. f))
     (mapcar
      #'file-name-sans-extension
      (scm-append-map
       #'(lambda (dir)
           (let ((len (length dir)))
             (mapcar #'(lambda (f) (substring f (+ 1 len)))
                     (scm-directory-tree-files dir t "\\.scm"))))
       (cons version-dir (cons site-dir other-dirs)))))))


;;TODO consider dynamic-load
(defun gauche-module-exports (mod)
  (let* ((file (concat (subst-char-in-string ?. ?/ (symbol-name mod)) ".scm"))
         (dir
          (scm-find-file-in-path
           file
	   (gauche-load-path))))
    (when dir
      (let (syms modules)
	(scm-with-find-file (concat dir "/" file)
	  (setq syms (gauche-exported-symbols))
	  (setq modules (gauche-base-modules)))
	;;TODO merge derived module to base module
	(mapc
	 (lambda (mod)
	   (setq syms (append (nth 1 (gauche-module-exports mod)) syms)))
	 modules)
	(list (concat dir "/" file) syms)))))

(defun gauche-base-modules ()
  (let (mod modules)
    (save-excursion
      (goto-char (point-min))
      ;;TODO in string
      (while (re-search-forward "(extend\\b" nil t)
	(unless (scm-in-string-p)
	  (ignore-errors
	    (while (setq mod (read (current-buffer)))
	      (setq modules (cons mod modules)))))))
    (nreverse modules)))

(defun gauche-exported-symbols ()
  (let ((env (scm-current-globals))
	(exports (gauche-current-exports t)))
    ;; if source file execute dynamic load.
    ;; global definition (env) will be null.
    (mapcar
     (lambda (x)
       (or (assq x env)
	   (cons x nil)))
     exports)))

(defun gauche-environ-load-path ()
  ;;TODO windows native version gosh
  (if (memq system-type '(windows-nt))
      (let ((path (split-string (or (getenv "GAUCHE_LOAD_PATH") "") path-separator)))
	(mapcar 'scm-cygwin-path-to-unix-path path))
    (split-string (or (getenv "GAUCHE_LOAD_PATH") "") path-separator)))

(defcustom gauche-program-name "gosh"
  "Gauche program name.")

(defvar gauche-load-path nil)

(defun gauche-load-path ()
  (or gauche-load-path
      (setq gauche-load-path (gauche-exact-load-path))))

(defun gauche-exact-load-path ()
  (let ((list '())
	(args nil))
    (setq args (list "-e" "(map print *load-path*)"))
    (with-temp-buffer
      (apply 'call-process gauche-program-name nil (current-buffer) nil args)
      (goto-char (point-min))
      (while (not (eobp))
	(setq list (cons (buffer-substring (line-beginning-position) (line-end-position)) list))
	(forward-line 1))
      list)))

(scm-set-alist 'scm-load-path-functions 'gauche 'gauche-load-path)

(defun gauche-current-exports (&optional only-current)
  (let ((res '()))
    (save-excursion
      (goto-char (point-min))
      (or (ignore-errors (end-of-defun) (beginning-of-defun) t)
          (re-search-forward "^(" nil t)
          (goto-char (point-max)))
      (while (not (eobp))
        (when (and (eq ?\( (char-syntax (char-after)))
                   (eq ?w (char-syntax (char-after (1+ (point))))))
          (let ((sym (save-excursion (forward-char) (scm-symbol-at-point))))
            (case sym
              ((declare define-module)
               (let ((decls (scm-nth-sexp-at-point 0)))
                 (when (and (not only-current) (assq 'extend decls))
                   (let ((parents (cdr (assq 'extend decls))))
                     (setq res (nconc (mapcar #'car
                                              (scm-append-map
                                               #'scm-module-exports
                                               parents))
                                      res))))
                 (cond
                  ((and (listp decls) (assq 'export decls))
                   (setq res (nconc (cdr (assq 'export decls)) res)))
                  ((and (listp decls) (assq 'export-all decls))
                   (goto-char (point-max))))))
              ((export)
               (setq res (nconc (cdr (scm-nth-sexp-at-point 0)) res)))
              ((export-all)
               (goto-char (point-max)))
              ((extend)
               (when (not only-current)
                 (let ((parents (cdr (scm-nth-sexp-at-point 0))))
                   (setq res (nconc (mapcar #'car
                                            (scm-append-map
                                             #'scm-module-exports
                                             parents))
                                    res)))))
              ((module)
               (forward-char)
               (forward-sexp)
               (let ((x (scm-nth-sexp-at-point 0)))
                 (cond
                  ((eq '* x)
                   (goto-char (point-max)))
                  ((listp x)
                   (setq res
                         (nconc (remove-if-not #'symbolp (cdr x)) res))))))
              )))
        (scm-goto-next-top-level)))
    res))



(scm-set-alist 'scm-module-exports-internal-functions 'gauche 'gauche-module-exports)
(scm-set-alist 'scm-current-exports-functions 'gauche 'gauche-current-exports)
(scm-set-alist 'scm-eldoc-highlight-sexp-functions 'gauche 'gauche-eldoc-highlight-sexp)



(provide 'gauche-env)

;;; gauche-env.el ends here
