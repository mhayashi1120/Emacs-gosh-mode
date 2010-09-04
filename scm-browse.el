;;; scm-browse.el --- 22044586035Programming language gauche browsing utilities.

;;TODO rename?

;;; Commentary:
;; 

;;; Code:


;;
;; font-lock
;;


(scm-case-defun scm-font-lock-keywords (arg)
  )

(font-lock-add-keywords
 'scheme-mode
 '((scm-font-lock-keywords . font-lock-keyword-face)))

;;
;; eldoc
;;

(defun scm-base-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((string list) (car x))
      ((set) (or (cadr x) (car x)))
      ((flags) 'integer)
      ((lambda) 'procedure)
      ((syntax) 'syntax)
      (t x))))

(defun scm-eldoc-canonicalize-order (ls)
  ;; put optional arguments inside brackets (via a vector)
  (if (memq :optional ls)
      (let ((res '())
	    (opts '())
	    (kwds '())
	    item)
	(while ls
	  (setq item (car ls))
	  (setq ls (cdr ls))
	  (if (keywordp item)
	      (case item
		(:optional
		 (while (and (consp ls) 
			     (not (keywordp (car ls))))
		   (setq opts (cons (car ls) opts))
		   (setq ls (cdr ls))))
		(:key
		 (unless (consp kwds)
		   (setq kwds (cons item kwds)))
		 (while (and (consp ls) 
			     (not (keywordp (car ls))))
		   (setq kwds (cons (car ls) kwds))
		   (setq ls (cdr ls)))))
	    (setq res (cons item res))))
	(append (nreverse res)
		(mapcar #'vector (nreverse opts))
		(nreverse kwds)))
    ls))

(scm-case-defun scm-eldoc-highlight-sexp (sexp highlight)
  "TODO"
  (let ((index (nth 0 highlight))
	(sym (nth 1 highlight))
	(prev-sym (nth 2 highlight))
	target-exp)
    (setq target-exp (nth index sexp))
    (unless target-exp
      (scm-aif (scm-eldoc-sexp-rest-arg sexp)
	  (setq target-exp it)))
    (concat "("
	    (mapconcat
	     'identity
	     (mapcar
	      (lambda (exp)
		(let ((str (scm-object-to-string exp)))
		  (when (eq target-exp exp)
		    (propertize str 'face 'eldoc-highlight-function-argument))
		  str))
	      sexp)
	     " ")
	    ")")))

(defun scm-eldoc-sexp-to-string (sexp &optional highlight)
  (if (not highlight)
      (scm-object-to-string sexp)
    (scm-eldoc-highlight-sexp sexp highlight)))

(defun scm-eldoc-sexp-rest-arg (sexp)
  (catch 'found
    (mapc
     (lambda (x)
       (when (and (vectorp x)
		  (> (length x) 0) 
		  (string-match "\\.\\.\\.$" (symbol-name (aref x 0))))
	 (throw 'found x)))
     sexp)
    nil))

(defun scm-eldoc-translate-dot-cell (ls)
  (let ((res '()))
    (while (consp ls)
      (setq res (cons (car ls) res))
      (setq ls (cdr ls)))
    (when (not (null ls))
      (setq res (cons (vector (intern (concat (scm-object-to-string ls) "..."))) res)))
    (reverse res)))

(defun scm-eldoc-print-current-symbol-info ()
  (let* ((sym0 (ignore-errors 
		 (save-excursion (backward-sexp) (scm-symbol-at-point))))
	 (prev-sym0 (ignore-errors 
		      (save-excursion (backward-sexp 2) (scm-keyword-at-point))))
	 (sym (eldoc-current-symbol))
         (fnsym0 (eldoc-fnsym-in-current-sexp))
         (fnsym (if (consp fnsym0) (car fnsym0) fnsym0))
	 (fnpos (if (consp fnsym0) (cadr fnsym0) 0))
         (env (save-excursion
                (if (scm-in-string-p) (scm-beginning-of-string))
                (scm-current-env)))
         (spec (or (and fnsym (scm-env-lookup env fnsym))
                   (and sym (scm-env-lookup env sym)))))
    (cond
     ((and (consp spec)
	   (consp (cdr spec)))
      (let ((type (cadr spec)))
	(concat
	 (cond
	  ((nth 3 spec)
	   "")
	  ((and (consp type)
		(memq (car type) '(syntax lambda)))
	   (concat
	    (if (eq (car type) 'syntax)
		"syntax: "
	      "")
	    (scm-eldoc-sexp-to-string
	     (cons (car spec)
		   (scm-eldoc-canonicalize-order
		    (mapcar #'scm-base-type
			    (scm-eldoc-translate-dot-cell
			     (cadr type)))))
	     (list fnpos sym0 prev-sym0))
	    (if (and (consp (cddr type))
		     (not (memq (caddr type) '(obj object))))
		(concat " => " (scm-eldoc-sexp-to-string (caddr type)))
	      "")))
	  ((and (consp type) (eq (car type) 'special))
	   (scm-eldoc-sexp-to-string (car spec)))
	  (t
	   (scm-eldoc-sexp-to-string type)))
	 (if (and (not (nth 3 spec)) (nth 4 spec)) " - " "")
	 (or (nth 4 spec) ""))))
     ((and (consp spec) (null (cdr spec)))
      ;;TODO more information.
      "parameter or alias"))))


(provide 'scm-browse)

;;; scm-browse.el ends here
