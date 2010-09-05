;;; gauche-browse.el --- Programming language gauche browsing source tools.


;;; Commentary:
;; 

;;; Code:

(require 'scm-env)
(require 'scm-browse)
(require 'eldoc nil t)

(defun gauche-jump-to-definition (definition)
  (let ((first (point)))
    (goto-char (point-min))
    (if (re-search-forward (format "^[ \t]*(def.*\\_<%s\\_>" definition) nil t)
	(forward-line 0)
      (goto-char first)
      nil)))

(defun gauche-jump-to-module ()
  (interactive)
  (let* ((symname (thing-at-point 'symbol))
	 (sym (intern symname)))
    (catch 'found
      (when (assq sym (scm-current-globals))
      	(gauche-jump-to-definition sym)
      	(throw 'found t))
      (mapc
       (lambda (f)
      	 (when (memq sym (gauche-exports-functions f))
	   ;;TODO opend or not
      	   (find-file f)
      	   (when (gauche-jump-to-definition sym)
	     (throw 'found t))))
       (gauche-guessed-modules symname))
      (message "Not found definition %s" symname))))

;;TODO parse use, import, require
(defun gauche-guessed-modules (symbol)
  (let ((sym (or (and (stringp symbol) symbol)
		 (symbol-name symbol)))
	prefix first-candidate second-candidate)
    (unless (string-match "^\\([^-]+\\)" sym)
      (error "Unable find"))
    (setq prefix (match-string 1 sym))
    (mapc
     (lambda (m)
       (let ((mname (symbol-name (car m))))
	 (if (member prefix (split-string mname "[.]"))
	     (setq first-candidate (cons (cadr m) first-candidate))
	   (setq second-candidate (cons (cadr m) second-candidate)))))
     *scm-complete-module-cache*)
    (append first-candidate second-candidate)))

(defun gauche-exports-functions (mod-file)
  (scm-with-find-file mod-file
    (scm-current-exports)))

(defun gauche-jump-to-info (symbol-name)
  (interactive 
   (let ((sym (thing-at-point 'symbol)))
     (list sym)))
  (let (point)
    (save-window-excursion
      ;;TODO
      (info "gauche-refj.info")
      (Info-search (format "-- \\(Function\\|Macro\\|Special Form\\): %s " symbol-name))
      (setq point (cons (current-buffer) (line-beginning-position))))
    (unless point
      (error "%s not found." symbol-name))
    (switch-to-buffer (car point))
    (goto-char (cdr point))))

;;
;; font-lock
;;

(defcustom gauche-user-procedure-regexp nil
  "*User defined keyword regexp that is faced `font-lock-keyword-face'.")

(defun gauche-font-lock-keywords (bound)
  (and (not (featurep 'quack))
       (or (re-search-forward (concat "(\\(" gauche-defined-procedure-keyword-regexp "\\)\\_>") bound t)
	   (re-search-forward (concat "(\\(" gauche-user-procedure-regexp "\\)\\_>") bound t))))

(scm-set-alist 'scm-font-lock-keywords-functions 'gauche 'gauche-font-lock-keywords)

;;TODO
;; (defconst gauche-font-lock-define-variable-regexp
;;   (concat "(\\(define-constant\\|define-values\\)\\s-+\\(\\sw+\\)"))

;; (defun gauche-font-lock-defined-variables (bound)
;;   (and (not (featurep 'quack))
;;        (re-search-forward gauche-font-lock-define-variable-regexp bound t)))

;; (scm-set-alist 'scm-font-lock-variables-functions 'gauche 'gauche-font-lock-defined-variables)



;;
;; eldoc
;;

(defun gauche-eldoc-highlight-sexp (sexp highlight)
  (let ((index (nth 0 highlight))
	(sym (nth 1 highlight))
	(prev-sym (nth 2 highlight))
	(real-sexp (gauche-eldoc-normalize-fn-sexp sexp))
	target-exp)
    (when (or (keywordp sym)
	      (keywordp prev-sym))
      (let ((key 
	     (intern-soft (substring (symbol-name 
				      (or 
				       (and (keywordp sym) sym)
				       (and (keywordp prev-sym) prev-sym)))
				     1))))
	(setq target-exp (assq key (cdr (memq :key sexp))))))
    (unless target-exp
      (setq target-exp (nth index real-sexp)))
    (unless target-exp
      (scm-aif (scm-eldoc-sexp-rest-arg real-sexp)
	  (setq target-exp it)))
    (concat "("
	    (mapconcat
	     'identity
	     (mapcar
	      (lambda (exp)
		(let ((str (scm-object-to-string exp)))
		  (when (eq target-exp exp)
		    (add-text-properties 0 (length str) 
					 (list 'face 'eldoc-highlight-function-argument)
					 str))
		  str))
	      sexp)
	     " ")
	    ")")))

(defun gauche-eldoc-normalize-fn-sexp (sexp)
  (let (ret ignore)
    (mapc
     (lambda (exp)
       (cond
	((eq exp :key)
	 (setq ignore t))
	((eq exp :optional)
	 (setq ignore nil))
	(ignore )
	(t
	 (setq ret (cons exp ret)))))
     sexp)
    (nreverse ret)))



(provide 'gauche-browse)

;;; gauche-browse.el ends here
