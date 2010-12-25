;;; gauche-mode.el --- Programming language gauche browsing source tools.


;;; Commentary:
;; 

;;; Code:

(require 'eldoc nil t)
(require 'gauche-const)
(require 'gauche-refactor)

(defcustom gauche-default-command "gosh"
  "Gauche program name.")

(defun gauche-call-command-to-string (command &rest args)
  (with-temp-buffer
    (apply 'call-process command nil (current-buffer) nil args)
    (buffer-string)))

(defvar gauche-default-version nil)
(defvar gauche-default-repo-path nil)
(defvar gauche-default-site-repo-path nil)

(defvar gauche-cached-load-path-alist nil)

(defun gauche-default-initialize ()
  (setq gauche-default-version
        (let ((ver (gauche-call-command-to-string gauche-default-command "-V")))
          (when (string-match "version[ \t]+\\([0-9][0-9.]+\\)" ver)
            (match-string 1 ver))))
  (setq gauche-default-repo-path
        ;; TODO not var to function?
        (or (let* ((res (gauche-call-command-to-string "gauche-config" "--syslibdir"))
                   (res (substring res 0 -1)))
              (and res (file-directory-p res)
                   (let* ((dir (file-name-directory res))
                          (dir2 (file-name-directory
                                 (directory-file-name dir))))
                     (directory-file-name dir2))))
            (car (remove-if-not 'file-directory-p
                                '("/usr/share/gauche"
                                  "/usr/local/share/gauche"
                                  "/opt/share/gauche"
                                  "/opt/local/share/gauche")))

            "/usr/local/share/gauche"))
  (setq gauche-default-site-repo-path
        (concat gauche-default-repo-path "/site/lib")))

;;TODO dummy
(defun gauche-available-modules (&optional dummy)
  (let ((version-dir
         (concat
	  ;;TODO version specific? get gosh ver?
          (car (directory-files gauche-default-repo-path t "^[0-9][0-9.]+\\(?:_pre[0-9]+\\)?$"))
          "/lib"))
        (site-dir gauche-default-site-repo-path)
        (other-dirs
         (remove-if-not
          (lambda (d) (and (not (equal d "")) (file-directory-p d)))
          (gauche-environ-load-path))))
    (setq other-dirs
          (mapcar 'directory-file-name other-dirs))
    (mapcar
     (lambda (f) 
       (subst-char-in-string ?/ ?. f))
     (mapcar
      'file-name-sans-extension
      (gauche-append-map
       (lambda (dir)
         (let ((len (length dir)))
           (mapcar 
            (lambda (f) (substring f (+ 1 len)))
            (gauche-directory-tree-files dir t "\\.scm"))))
       (cons version-dir (cons site-dir other-dirs)))))))

;;TODO consider dynamic-load
(defun gauche-parse-module-exports (mod)
  (let* ((file (concat (subst-char-in-string ?. ?/ (symbol-name mod)) ".scm"))
         (dir
          (gauche-scheme-find-file-in-path
           file
	   (gauche-load-path))))
    (when dir
      (let (syms modules)
	(gauche-with-find-file (concat dir "/" file)
          (setq syms (gauche-parse-exported-symbols))
          (setq modules (gauche-parse-base-modules)))
	;;TODO merge derived module to base module
	(mapc
	 (lambda (mod)
	   (setq syms (append (nth 1 (gauche-parse-module-exports mod)) syms)))
	 modules)
	(list (concat dir "/" file) syms)))))

(defun gauche-parse-base-modules ()
  (let (mod modules)
    (save-excursion
      (goto-char (point-min))
      ;;TODO in string
      (while (re-search-forward "(extend\\b" nil t)
	(unless (gauche-in-string-p)
	  (ignore-errors
	    (while (setq mod (read (current-buffer)))
	      (setq modules (cons mod modules)))))))
    (nreverse modules)))

(defun gauche-parse-exported-symbols ()
  (let ((env (gauche-parse-current-globals))
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
	(mapcar 'gauche-cygwin-path-to-unix-path path))
    (split-string (or (getenv "GAUCHE_LOAD_PATH") "") path-separator)))

(defvar gauche-load-path nil)

(defun gauche-load-path ()
  (or gauche-load-path
      (setq gauche-load-path (gauche-exact-load-path))))

(defun gauche-exact-load-path ()
  (let ((list '())
	(args nil))
    (setq args (list "-e" "(map print *load-path*)"))
    (with-temp-buffer
      (apply 'call-process gauche-default-command nil (current-buffer) nil args)
      (goto-char (point-min))
      (while (not (eobp))
	(setq list (cons (buffer-substring (line-beginning-position) (line-end-position)) list))
	(forward-line 1))
      list)))

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
          (let ((sym (save-excursion (forward-char) (gauche-parse-symbol-at-point))))
            (case sym
              ((declare define-module)
               (let ((decls (gauche-nth-sexp-at-point 0)))
                 (when (and (not only-current) (assq 'extend decls))
                   (let ((parents (cdr (assq 'extend decls))))
                     (setq res (nconc (mapcar 'car
                                              (gauche-append-map
                                               'gauche-scheme-module-exports
                                               parents))
                                      res))))
                 (cond
                  ((and (listp decls) (assq 'export decls))
                   (setq res (nconc (cdr (assq 'export decls)) res)))
                  ((and (listp decls) (assq 'export-all decls))
                   (goto-char (point-max))))))
              ((export)
               (setq res (nconc (cdr (gauche-nth-sexp-at-point 0)) res)))
              ((export-all)
               (goto-char (point-max)))
              ((extend)
               (when (not only-current)
                 (let ((parents (cdr (gauche-nth-sexp-at-point 0))))
                   (setq res (nconc (mapcar 'car
                                            (gauche-append-map
                                             'gauche-scheme-module-exports
                                             parents))
                                    res)))))
              ((module)
               (forward-char)
               (forward-sexp)
               (let ((x (gauche-nth-sexp-at-point 0)))
                 (cond
                  ((eq '* x)
                   (goto-char (point-max)))
                  ((listp x)
                   (setq res
                         (nconc (remove-if-not 'symbolp (cdr x)) res))))))
              )))
        (gauche-goto-next-top-level)))
    res))

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
      (when (assq sym (gauche-parse-current-globals))
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
     gauche-cache-module-exports)
    (append first-candidate second-candidate)))

(defun gauche-exports-functions (mod-file)
  (gauche-with-find-file mod-file
    (gauche-current-exports)))

(defun gauche-jump-to-info (symbol-name)
  (interactive 
   (let ((sym (thing-at-point 'symbol)))
     (list sym)))
  (let (point)
    (save-window-excursion
      ;;TODO
      (info "gauche-refj.info")
      (Info-search (format "-- \\(Generic function\\|Function\\|Macro\\|Special Form\\|Variable\\): %s " (regexp-quote symbol-name)))
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

(font-lock-add-keywords
 'gauche-mode
 '((gauche-font-lock-keywords 1 font-lock-keyword-face)))



;; syntax

(defconst gauche-font-lock-syntactic-keywords
  `(
    (,gauche-regexp-literal-regexp (1 (6)) (2 (7)) (4 (7)))
    ))

(defun gauche-syntax-table-reset ()
  (let ((modified (buffer-modified-p))
        buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward gauche-regexp-literal-regexp nil t)
        (gauche-syntax-table-set-properties (match-beginning 0) (match-end 0))))
    (set-buffer-modified-p modified)))

(defun gauche-syntax-table-put-property (beg end value)
  (put-text-property beg end 'syntax-table value (current-buffer)))

(defun gauche-syntax-table-set-properties (beg end)
  (let ((curpos beg)
        (state 0))
    ;; (remove-text-properties beg end 'syntax-table)

    (while (<= curpos end)
      (cond
       ((= state 0)
        (when (= (char-after curpos) ?#)
          ;; (6) = expression prefix, (3) = symbol
          (gauche-syntax-table-put-property curpos (1+ curpos) '(6))) 
        (setq state (+ 1 state)))

       ((= state 1)
        (when (= (char-after curpos) ?/)
          ;; (7) = string quote
          (gauche-syntax-table-put-property curpos (1+ curpos) '(7))) 
        (setq state (+ 1 state)))

       ((= state 2)
        (cond
         ;; handle backslash inside the string
         ((= (char-after curpos) ?/)
          ;; (1) = punctuation, (2) = word
          (if (= (char-before curpos) ?\\)
              (gauche-syntax-table-put-property curpos (+ curpos 2) '(2))
            (gauche-syntax-table-put-property curpos (1+ curpos) '(7))))

         ;; everything else
         (t
          nil))))
      ;; next char
      (setq curpos (+ curpos 1)))))



(defvar gauche-mode-map nil)

(unless gauche-mode-map
  (let ((map (copy-keymap scheme-mode-map)))
    
    ;;TODO fix key conventions
    (define-key map "\M-\C-i" 'gauche-smart-complete)
    (define-key map "\C-cj" 'gauche-jump-to-module)
    (define-key map "\C-ch" 'gauche-jump-to-info)
    (define-key map "\C-cR" 'gauche-refactor-rename-symbol)
    (define-key map "\C-c\er" 'gauche-refactor-rename-symbol-afaiui)

    (setq gauche-mode-map map)))


(defvar gauche-mode-hook nil)

;;TODO sche-mode-hook -> gauche-mode-hook
(add-hook 'gauche-mode-hook
	  (lambda ()
	    (make-local-variable 'eldoc-documentation-function)
	    (setq eldoc-documentation-function 'gauche-eldoc-print-current-symbol-info)
	    (eldoc-mode)))


(define-derived-mode gauche-mode scheme-mode "Gauche"
  "Gauche editing mode"
  (set (make-local-variable 'font-lock-syntactic-keywords) 
       gauche-font-lock-syntactic-keywords)
  (gauche-syntax-table-reset)
  (use-local-map gauche-mode-map)
  (run-mode-hooks 'gauche-mode-hook))



;;
;; eldoc
;;

;;TODO http://practical-scheme.net/chaton/gauche/a/2010/09/10#entry-4c8a78c0-8c5fd
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
      (gauche-aif (gauche-eldoc-sexp-rest-arg real-sexp)
	  (setq target-exp it)))
    (concat "("
	    (mapconcat
	     'identity
	     (mapcar
	      (lambda (exp)
		(let ((str (gauche-object-to-string exp)))
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

(defun gauche--base-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((string list) (car x))
      ((set) (or (cadr x) (car x)))
      ((flags) 'integer)
      ((lambda) 'procedure)
      ((syntax) 'syntax)
      (t x))))

(defun gauche-eldoc--canonicalize-order (ls)
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
		(mapcar 'vector (nreverse opts))
		(nreverse kwds)))
    ls))

(defun gauche-eldoc--sexp-to-string (sexp &optional highlight)
  (if (not highlight)
      (gauche-object-to-string sexp)
    (gauche-eldoc-highlight-sexp sexp highlight)))

(defun gauche-eldoc-sexp-rest-arg (sexp)
  (catch 'found
    (mapc
     (lambda (x)
       (when (and (vectorp x)
		  (> (length x) 0) 
		  (string-match "\\.\\.\\.$" (symbol-name (aref x 0))))
	 (throw 'found x)))
     sexp)
    (let ((last (car (last sexp))))
      (when (string-match "\\.\\.\\.$" (symbol-name last))
        (throw 'found last)))
    nil))

(defun gauche-eldoc--translate-dot-cell (ls)
  (let ((res '()))
    (while (consp ls)
      (setq res (cons (car ls) res))
      (setq ls (cdr ls)))
    (when (not (null ls))
      (setq res (cons (vector (intern (concat (gauche-object-to-string ls) "..."))) res)))
    (reverse res)))

(defun gauche-eldoc-print-current-symbol-info ()
  (let* ((sym0 (ignore-errors 
		 (save-excursion (backward-sexp) (gauche-parse-symbol-at-point))))
	 (prev-sym0 (ignore-errors 
		      (save-excursion (backward-sexp 2) (gauche-keyword-at-point))))
	 (sym (eldoc-current-symbol))
         (fnsym0 (eldoc-fnsym-in-current-sexp))
         (fnsym (if (consp fnsym0) (car fnsym0) fnsym0))
	 (fnpos (if (consp fnsym0) (cadr fnsym0) 0))
         (env (save-excursion
                (if (gauche-in-string-p) (gauche-beginning-of-string))
                (gauche-scheme-current-env)))
         (spec (or (and fnsym (gauche-env-lookup env fnsym))
                   (and sym (gauche-env-lookup env sym)))))
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
	    (gauche-eldoc--sexp-to-string
	     (cons (car spec)
		   (gauche-eldoc--canonicalize-order
		    (mapcar 'gauche--base-type
			    (gauche-eldoc--translate-dot-cell
			     (cadr type)))))
	     (list fnpos sym0 prev-sym0))
	    (if (and (consp (cddr type))
		     (not (memq (caddr type) '(obj object))))
		(concat " => " (gauche-eldoc--sexp-to-string (caddr type)))
	      "")))
	  ((and (consp type) (eq (car type) 'special))
	   (gauche-eldoc--sexp-to-string (car spec)))
	  (t
	   (gauche-eldoc--sexp-to-string type)))
	 (if (and (not (nth 3 spec)) (nth 4 spec)) " - " "")
	 (or (nth 4 spec) ""))))
     ((and (consp spec) (null (cdr spec)))
      ;;TODO more information.
      "parameter or alias"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special lookups (XXXX add more impls, try to abstract better)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defun gauche-filter (pred env)
  (mapcar 'car
          (apply 'concatenate
                 'list
                 (mapcar (lambda (e) (remove-if-not pred e)) env))))

(defun gauche-append-map (proc init-ls)
  (if (null init-ls)
      '()
    (let* ((ls (reverse init-ls))
           (res (funcall proc (pop ls))))
      (while (consp ls)
        (setq res (append (funcall proc (pop ls)) res)))
      res)))

(defun gauche-flatten (ls)
  (cond
   ((consp ls) (cons (car ls) (gauche-flatten (cdr ls))))
   ((null ls) '())
   (t (list ls))))

(defun gauche-in-string-p ()
  (let ((orig (point)))
    (save-excursion
      (goto-char (point-min))
      (let ((parses (parse-partial-sexp (point) orig)))
        (nth 3 parses)))))

(defun gauche-beginning-of-sexp ()
  (unless (bobp)
    (let ((syn (char-syntax (char-before (point)))))
      (if (or (eq syn ?\()
              (and (eq syn ?\") (gauche-in-string-p)))
          (forward-char -1)
        (forward-sexp -1)))))

(defun gauche-scheme-find-file-in-path (file path)
  (car (remove-if-not
        (lambda (dir) (file-exists-p (concat dir "/" file)))
        path)))

(put 'gauche-with-find-file 'lisp-indent-function 1)
(defmacro gauche-with-find-file (path-expr &rest body)
  (let ((path (gensym "path"))
        (buf (gensym "buf"))
        (res (gensym "res")))
    `(let* ((,path (file-truename ,path-expr))
            (,buf (get-file-buffer ,path)))
       (with-current-buffer (or ,buf (find-file-noselect ,path t))
	 (let (,res)
	   (unwind-protect
	       (setq ,res (ignore-errors (save-excursion ,@body)))
	     (unless ,buf 
	       (kill-buffer (current-buffer))))
           ,res)))))

(defun gauche-directory-tree-files (init-dir &optional full match)
  (let ((res '())
        (stack (list init-dir)))
    (while (consp stack)
      (let* ((dir (pop stack))
             (files (cddr (directory-files dir full))))
        (setq res (append (if match
                              (remove-if-not
                               (lambda (f) (string-match match f))
                               files)
                            files)
                          res))
        (setq stack
              (append
               (remove-if-not 'file-directory-p
                              (if full
                                  files
                                (mapcar (lambda (f) (concat dir "/" f))
                                        files)))
               stack))))
    res))

(defmacro gauche-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'gauche-aif 'lisp-indent-function 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alist utilities

(defun gauche-put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST.

This function come from apel"
  (let ((elm (assoc key alist)))
    (if elm
	(progn
	  (setcdr elm value)
	  alist)
      (cons (cons key value) alist))))

(defun gauche-set-alist (symbol key value)
  "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE.

This function come from apel"
  (or (boundp symbol)
      (set symbol nil))
  (set symbol (gauche-put-alist key value (symbol-value symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sexp manipulation

;; returns current argument position within sexp
(defun gauche-beginning-of-current-sexp-operator ()
  (let ((pos 0))
    (skip-syntax-backward "w_")
    (while (and (not (bobp)) (not (eq ?\( (char-before))))
      (gauche-beginning-of-sexp)
      (incf pos))
    pos))

(defun gauche-beginning-of-next-sexp ()
  (forward-sexp 2)
  (backward-sexp 1))

(defun gauche-beginning-of-string ()
  (interactive)
  (search-backward "\"" nil t)
  (while (and (> (point) (point-min)) (eq ?\\ (char-before)))
    (search-backward "\"" nil t)))

;; for the enclosing sexp, returns a cons of the leading symbol (if
;; any) and the current position within the sexp (starting at 0)
;; (defun gauche-enclosing-sexp-prefix ()
;;   (save-excursion
;;     (let ((pos (gauche-beginning-of-current-sexp-operator)))
;;       (cons (gauche-parse-symbol-at-point) pos))))

(defun gauche-enclosing-2-sexp-prefixes ()
  (save-excursion
    (let* ((pos1 (gauche-beginning-of-current-sexp-operator))
           (sym1 (gauche-parse-symbol-at-point)))
      (backward-char)
      (or
       (ignore-errors
         (let ((pos2 (gauche-beginning-of-current-sexp-operator)))
           (list sym1 pos1 (gauche-parse-symbol-at-point) pos2)))
       (list sym1 pos1 nil 0)))))

;; sexp-at-point is always fragile, both because the user can input
;; incomplete sexps and because some scheme sexps are not valid elisp
;; sexps.  this is one of the few places we use it, so we're careful
;; to wrap it in ignore-errors.
(defun gauche-nth-sexp-at-point (n)
  (ignore-errors
    (save-excursion
      (forward-sexp (+ n 1))
      (let ((end (point)))
        (forward-sexp -1)
        (car (gauche-read-from-string (buffer-substring (point) end)))))))

;;FIXME TODO regex literal
(defun gauche-read-from-string (string)
  (condition-case err
      (read-from-string string)
    (invalid-read-syntax 
     (if (string= (cadr err) "#")
	 ;;FIXME not concern about in string?
	 ;; Probablly allmost case is ok.
	 (read-from-string (replace-regexp-in-string "#" "\\\\#" string))
       (error "Assert")))))

(defun gauche-parse-symbol-at-point ()
  (save-excursion
    (skip-syntax-backward "w_")
    (let ((start (point)))
      (skip-syntax-forward "w_")
      (and (< start (point))
           (intern (buffer-substring start (point)))))))

(defun gauche-goto-next-top-level ()
  (let ((here (point)))
    (or (ignore-errors (end-of-defun) (end-of-defun)
                       (beginning-of-defun)
                       (< here (point)))
        (progn (forward-char)
               (and (re-search-forward "^(" nil t)
                    (progn (backward-char 1) t)))
        (goto-char (point-max)))))

(defun gauche-keyword-at-point ()
  (let ((sym (gauche-parse-symbol-at-point)))
    (when (and sym
	       (keywordp sym))
      sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable extraction

(defun gauche-parse-sexp-type-at-point (&optional env)
  (case (char-syntax (char-after))
    ((?\()
     (forward-char 1)
     (if (eq ?w (char-syntax (char-after)))
         (let ((op (gauche-parse-symbol-at-point)))
           (cond
            ((eq op 'lambda)
             (let ((params
                    (gauche-nth-sexp-at-point 1)))
               `(lambda ,params)))
            (t
             (let ((spec (gauche-env-lookup env op)))
               (and spec
                    (consp (cadr spec))
                    (eq 'lambda (caadr spec))
                    (cddadr spec)
                    (car (cddadr spec)))))))
       nil))
    ((?\")
     'string)
    ((?\w)
     (if (string-match "[0-9]" (string (char-after)))
         'number
       nil))
    (t
     nil)))

(defun gauche-parse-let-vars-at-point (&optional env limit loopp)
  (let ((end (min (or limit (point-max))
                  (or (ignore-errors
                        (save-excursion (forward-sexp) (point)))
                      (point-min))))
        (vars '()))
    (forward-char 1)
    (while (< (point) end)
      (when (eq ?\( (char-after))
        (save-excursion
          (forward-char 1)
          (if (and loopp (looking-at "\\(for\\|let\\|with\\)\\>"))
              (gauche-beginning-of-next-sexp))
          (if (eq ?w (char-syntax (char-after)))
              (let* ((sym (gauche-parse-symbol-at-point))
                     (type (and (not loopp)
                                (ignore-errors
                                  (gauche-beginning-of-next-sexp)
                                  (gauche-parse-sexp-type-at-point env)))))
                (push (if type (list sym type) (list sym)) vars)))
          (when loopp
            (while (and (< (point) end)
                        (ignore-errors
                          (gauche-beginning-of-next-sexp)
                          (eq ?w (char-syntax (char-after)))))
              (push (list (gauche-parse-symbol-at-point)) vars)))))
      (unless (ignore-errors (let ((here (point)))
                               (gauche-beginning-of-next-sexp)
                               (> (point) here)))
        (goto-char end)))
    (reverse vars)))

(defun gauche-parse-extract-match-clause-vars (x)
  (cond
   ((null x) '())
   ((symbolp x)
    (if (memq x '(_ ___ \.\.\.))
        '()
      (list (list x))))
   ((consp x)
    (case (car x)
      ((or not)
       (gauche-parse-extract-match-clause-vars (cdr x)))
      ((and)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (consp (caddr x))
                (not (memq (caaddr x)
                           '(= $ @ ? and or not quote quasiquote get! set!))))
           (cons (list (cadr x) (if (listp (caddr x)) 'list 'pair))
                 (gauche-parse-extract-match-clause-vars (cddr x)))
         (gauche-parse-extract-match-clause-vars (cddr x))))
      ((= $ @)
       (if (consp (cdr x)) (gauche-parse-extract-match-clause-vars (cddr x)) '()))
      ((\? ? ) ; XXXX this is a hack, the lone ? gets read as a char (space)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (symbolp (caddr x)))
           (cons (list (caddr x) (gauche-predicate->type (cadr x)))
                 (gauche-parse-extract-match-clause-vars (cdddr x)))
         (gauche-parse-extract-match-clause-vars (cddr x))))
      ((get! set!)
       (if (consp (cdr x)) (gauche-parse-extract-match-clause-vars (cadr x)) '()))
      ((quote) '())
      ((quasiquote) '())                ; XXXX
      (t
       (union (gauche-parse-extract-match-clause-vars (car x))
              (gauche-parse-extract-match-clause-vars (cdr x))))))
   ((vectorp x)
    (gauche-parse-extract-match-clause-vars (concatenate 'list x)))
   (t
    '())))

;; call this from the first opening paren of the match clauses
(defun gauche-parse-extract-match-vars (&optional pos limit)
  (let ((match-vars '())
        (limit (or limit
                   (save-excursion
                     (or
                      (ignore-errors (end-of-defun) (point))
                      (point-max))))))
    (save-excursion
      (while (< (point) limit)
        (let* ((end (ignore-errors (forward-sexp) (point)))
               (start (and end (progn (backward-sexp) (point)))))
          (cond
           ((and pos start end (or (< pos start) (> pos end)))
            (goto-char (if end (+ end 1) limit)))
           (t
            (forward-char 1)
            (let* ((pat (gauche-nth-sexp-at-point 0))
                   (new-vars (ignore-errors
                               (gauche-parse-extract-match-clause-vars pat))))
              (setq match-vars (append new-vars match-vars)))
            (goto-char (if (or pos (not end)) limit (+ end 1)))))))
      match-vars)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complete

(defgroup gauche-complete nil
  "Smart tab completion"
  :group 'scheme)

(defcustom gauche-complete-smart-indent-p t
  "Toggles using `scm-smart-indent' for `gauche-complete-or-indent'."
  :type 'boolean
  :group 'gauche-complete)

(defcustom gauche-complete-cache-p t
  "Toggles caching of module/load export information."
  :type 'boolean
  :group 'gauche-complete)

;; (defcustom gauche-complete-learn-syntax-p nil
;;   "Toggles parsing of syntax-rules macros for completion info."
;;   :type 'boolean
;;   :group 'gauche-complete)

(defvar *gauche-interleave-definitions-p* nil)

(defvar gauche-cache-module-exports '()
  "Each item is following list
\\(module-symbol module-file time exported-symbols [defined-symbols])

TODO key should be module-file?? multiple executable make complex.
"
  )

(defvar gauche-cache-file-globals '()
  "Each item is following list
\\(file-name time global-symbols)"
  )

;; TODO need????
(defvar gauche-processing-modules '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gauche-parse-current-local-vars (&optional env)
  (let ((vars '())
        (start (point))
        (limit (save-excursion (beginning-of-defun) (+ (point) 1)))
        (let-limit (save-excursion (gauche-beginning-of-sexp)
                                   (gauche-beginning-of-sexp)
                                   (point)))
        (scan-internal))
    (save-excursion
      (while (> (point) limit)
        (or (ignore-errors
              (progn
                (skip-chars-backward " \t\n" limit)
                (gauche-beginning-of-sexp)
                t))
            (goto-char limit))
        (when (and (> (point) (point-min))
                   (eq ?\( (char-syntax (char-before (point))))
                   (eq ?w (char-syntax (char-after (point)))))
          (setq scan-internal t)
          (let ((sym (gauche-parse-symbol-at-point)))
            (case sym
              ((lambda)
               (setq vars
                     (append
                      (mapcar 'list
                              (gauche-flatten (gauche-nth-sexp-at-point 1)))
                      vars)))
              ((match match-let match-let*)
               (setq vars
                     (append
                      (ignore-errors
                        (save-excursion
                          (let ((limit (save-excursion
                                         (cond
                                          ((eq sym 'match)
                                           (backward-char 1)
                                           (forward-sexp 1))
                                          (t
                                           (forward-sexp 2)))
                                         (point))))
                            (forward-sexp 2)
                            (if (eq sym 'match)
                                (forward-sexp 1))
                            (backward-sexp 1)
                            (if (not (eq sym 'match))
                                (forward-char 1))
                            (gauche-parse-extract-match-vars
                             (and (or (eq sym 'match) (< start limit)) start)
                             limit))))
                      vars)))
              ((let let* letrec letrec* let-syntax letrec-syntax
                    and-let* do loop)
               (or
                (ignore-errors
                  (save-excursion
                    (gauche-beginning-of-next-sexp)
                    (let* ((loop-name
                            (and (memq sym '(let loop))
                                 (eq ?w (char-syntax (char-after (point))))
                                 (prog1 (gauche-parse-symbol-at-point)
                                   (gauche-beginning-of-next-sexp))))
                           (let-vars
                            (gauche-parse-let-vars-at-point
                             env let-limit (eq sym 'loop))))
                      (if loop-name
                          ;; named let
                          (setq vars
                                (cons `(,loop-name (lambda ,(mapcar 'car let-vars)))
                                      (append let-vars vars)))
                        (setq vars (append let-vars vars))))
                    t))
                (goto-char limit)))
              ((let-values let*-values)
               (setq vars
                     (append (mapcar
                              'list
                              (gauche-append-map
                               'gauche-flatten
                               (remove-if-not 'consp
                                              (gauche-nth-sexp-at-point 1))))
                             vars)))
              ((receive defun defmacro)
               (setq vars
                     (append (mapcar 'list
                                     (gauche-flatten
                                      (gauche-nth-sexp-at-point 1)))
                             vars)))
              (t
               (if (string-match "^define\\(-.*\\)?" (symbol-name sym))
                   (let ((defs (save-excursion
                                 (backward-char)
                                 (gauche-parse-read-definitions))))
                     (setq vars
                           (append (gauche-append-map
                                    (lambda (x)
                                      (and (consp (cdr x))
                                           (consp (cadr x))
                                           (eq 'lambda (caadr x))
                                           (mapcar 'list
                                                   (gauche-flatten
                                                    (cadadr x)))))
                                    defs)
                                   (and (not (= 1 (current-column))) defs)
                                   vars)))
                 (setq scan-internal nil))))
            ;; check for internal defines
            (when scan-internal
              (ignore-errors
                (save-excursion
                  (forward-sexp
                   (+ 1 (if (numberp scan-internal) scan-internal 2)))
                  (backward-sexp)
                  (if (< (point) start)
                      (setq vars (append (gauche-scheme-current-definitions) vars))
                    ))))))))
    (reverse vars)))

(defun gauche-parse-extract-import-module-exports (sexp)
  (case (and (consp sexp) (car sexp))
    ((prefix prefix-in)
     (let* ((ids (gauche-parse-extract-import-module-exports (cadr sexp)))
            (prefix0 (caddr sexp))
            (prefix (if (symbolp prefix0) (symbol-name prefix0) prefix0)))
       (mapcar (lambda (x)
                 (cons (intern (concat prefix (symbol-name (car x))))
                       (cdr x)))
               ids)))
    ((prefix-all-except)
     (let ((prefix
            (if (symbolp (cadr sexp)) (symbol-name (cadr sexp)) (cadr sexp)))
           (exceptions (cddr sexp)))
       (mapcar (lambda (x)
                 (if (memq (car x) exceptions)
                     x
                   (cons (intern (concat prefix (symbol-name (car x))))
                         (cdr x))))
               (gauche-parse-extract-import-module-exports (caddr sexp)))))
    ((for for-syntax for-template for-label for-meta)
     (gauche-parse-extract-import-module-exports (cadr sexp)))
    ((rename rename-in)
     (let ((renames (cddr sexp)))
       (mapcar (lambda (x)
                 (cons (or (cadr (assq (car x) renames)) (car x)) (cdr x)))
               (gauche-parse-extract-import-module-exports (cadr sexp)))))
    ((except except-in)
     (remove-if (lambda (x) (memq (car x) (cddr sexp)))
                (gauche-parse-extract-import-module-exports (cadr sexp))))
    ((only only-in)
     (remove-if-not
      (lambda (x) (memq (car x) (cddr sexp)))
      (gauche-parse-extract-import-module-exports (cadr sexp))))
    ((import import-for-syntax require)
     (gauche-append-map 'gauche-parse-extract-import-module-exports (cdr sexp)))
    ((library)
     (if (and (stringp (cadr sexp)) (file-exists-p (cadr sexp)))
         (gauche-scheme-module-exports (intern (cadr sexp)))))
    ((lib)
     (if (and (equal "srfi" (caddr sexp))
              (stringp (cadr sexp))
              (string-match "^[0-9]+\\." (cadr sexp)))
         (gauche-scheme-module-exports
          (intern (file-name-sans-extension (concat "srfi-" (cadr sexp)))))
       (gauche-scheme-module-exports
        (intern (apply 'concat (append (cddr sexp) (list (cadr sexp))))))))
    (t
     (gauche-scheme-module-exports sexp))))

(defun gauche-parse-sexp-imports (sexp)
  (case (and (consp sexp) (car sexp))
    ((begin define-module)
     (gauche-append-map 'gauche-parse-sexp-imports (cdr sexp)))
    ((cond-expand)
     (gauche-append-map 'gauche-parse-sexp-imports
                     (gauche-append-map 'cdr (cdr sexp))))
    ((use require-extension)
     (gauche-append-map 'gauche-scheme-module-exports (cdr sexp)))
    ((import)
     (gauche-append-map 'gauche-parse-extract-import-module-exports (cdr sexp)))
    ((autoload)
     (unless (member (cadr sexp) gauche-processing-modules)
       (push (cadr sexp) gauche-processing-modules)
       (mapcar (lambda (x) (cons (if (consp x) (car x) x) '((lambda obj))))
               (cddr sexp))))
    ((load)
     (unless (member (cadr sexp) gauche-processing-modules)
       (push (cadr sexp) gauche-processing-modules)
       (and (stringp (cadr sexp))
	    ;;TODO gauche accepts relative path in *load-path*
            (file-exists-p (cadr sexp))
            (gauche-with-find-file (cadr sexp)
              (gauche-parse-current-globals)))))
    ((library module)
     (gauche-append-map 'gauche-parse-extract-import-module-exports
                     (remove-if (lambda (x)
                                  (memq (car x) '(import require)))
                                (cdr sexp))))
    ))

(defun gauche-module-symbol-p (sym)
  (memq sym '(use require require-extension begin cond-expand
                  module library define-module autoload load import)))

(defun gauche-skip-shebang ()
  ;; skip shebang if present
  (if (looking-at "#!")
      (forward-line)))

(defun gauche-parse-current-imports ()
  (let ((imports '())
        (gauche-processing-modules '()))
    (save-excursion
      (goto-char (point-min))
      (gauche-skip-shebang)
      ;; scan for module forms
      (while (not (eobp))
        (if (ignore-errors (forward-sexp) t)
            (let ((end (point))
                  (inside-p nil))
              (backward-sexp)
              (when (eq ?\( (char-after))
                (forward-char)
                (when (not (eq ?\( (char-after)))
                  (let ((sym (gauche-parse-symbol-at-point)))
                    (cond
                     ((memq sym '(module library))
                      (forward-sexp 3)
                      (setq inside-p t))
                     ((gauche-module-symbol-p sym)
                      (backward-char)
                      (ignore-errors
                        (setq imports
                              (append (gauche-parse-sexp-imports
                                       (gauche-nth-sexp-at-point 0))
                                      imports))))))))
              (unless inside-p (goto-char end)))
          ;; if an incomplete sexp is found, try to recover at the
          ;; next line beginning with an open paren
          (gauche-goto-next-top-level))))
    imports))

;; we should be just inside the opening paren of an expression
(defun gauche-parse-name-of-current-define ()
  (save-excursion
    (gauche-beginning-of-next-sexp)
    (if (eq ?\( (char-syntax (char-after)))
        (forward-char))
    (and (memq (char-syntax (char-after)) '(?\w ?\_))
         (gauche-parse-symbol-at-point))))

(defun gauche-parse-type-of-current-define ()
  (save-excursion
    (gauche-beginning-of-next-sexp)
    (cond
     ((eq ?\( (char-syntax (char-after)))
      `(lambda ,(cdr (gauche-nth-sexp-at-point 0))))
     (t
      (ignore-errors (gauche-beginning-of-next-sexp)
                     (gauche-parse-sexp-type-at-point))))))

(defun gauche-parse-applying-of-current-define ()
  (save-excursion
    (gauche-beginning-of-next-sexp)
    (gauche-beginning-of-next-sexp)
    (ignore-errors 
      `(lambda ,(gauche-nth-sexp-at-point 0)))))

;; we should be at the opening paren of an expression
(defun gauche-parse-read-definitions (&optional env)
  (save-excursion
    (let ((sym (ignore-errors (and (eq ?\( (char-syntax (char-after)))
                                   (progn (forward-char)
                                          (gauche-parse-symbol-at-point))))))
      (case sym
        ((define-syntax define-compiled-syntax defmacro define-macro)
         (list (list (gauche-parse-name-of-current-define) '(syntax))))
        ((define-method)
         (let ((name (gauche-parse-name-of-current-define))
               (type (gauche-parse-applying-of-current-define)))
           (list (if type (list name type) (list name)))))
        ((define define-constant)
         ;;TODO define-in-module define-values
         (let ((name (gauche-parse-name-of-current-define))
               (type (gauche-parse-type-of-current-define)))
           (list (if type (list name type) (list name)))))
        ((define-class)
         (list (list (gauche-parse-name-of-current-define) 'non-procedure)))
        ((define-record-type)
         (backward-char)
         (ignore-errors
           (let ((sexp (gauche-nth-sexp-at-point 0)))
             `((,(caaddr sexp) (lambda ,(cdaddr sexp)))
               (,(cadddr sexp) (lambda (obj)))
               ,@(gauche-append-map 
                  (lambda (x)
                    (if (consp x)
                        (if (consp (cddr x))
                            `((,(cadr x) (lambda (non-procedure)))
                              (,(caddr x)
                               (lambda (non-procedure val) undefined)))
                          `((,(cadr x) (lambda (non-procedure)))))))
                  (cddddr sexp))))))
        ((begin begin0)
         (forward-sexp)
         (gauche-scheme-current-definitions))
        (t
         '())))))

(defun gauche-scheme-in-defun-name ()
  (save-excursion
    (dotimes (i 2)
      (gauche-beginning-of-sexp)
      (unless (bobp)
        (backward-char)))
    (and (= 0 (current-column))
         (looking-at "(define")
         (point))))

(defun gauche-parse-file-globals (file)
  (gauche-with-find-file file
    (gauche-parse-current-globals)))

;; a little more liberal than -definitions, we try to scan to a new
;; top-level form (i.e. a line beginning with an open paren) if
;; there's an error during normal sexp movement
(defun gauche-parse-current-globals ()
  (let ((here (point))
        (skip (gauche-scheme-in-defun-name))
        (globals '())
        (end (point-max)))
    (save-excursion
      (goto-char (point-min))
      (or (ignore-errors (end-of-defun) (backward-sexp) t)
          (and (re-search-forward "^(" nil t) (progn (backward-char) t))
          (goto-char (point-max)))
      (while (< (point) end)
        (cond
         ((and (< (point) here) (looking-at "(\\(module\\|library\\)\\s-"))
          (let ((module-end (ignore-errors
                              (save-excursion (forward-sexp) (point)))))
            (cond
             ((or (not module-end) (< here module-end)) ; inside the module
              (setq globals '())
              (when module-end
                (setq end module-end))
              (forward-word 1)
              (forward-sexp 2)
              (gauche-beginning-of-next-sexp))
             (t ;; not inside the module, skip it altogether
              (forward-sexp 1)
              (gauche-goto-next-top-level)))))
         (t
          (unless (eq (point) skip)
            (setq globals
                  (append
                   (ignore-errors (gauche-parse-read-definitions))
                   globals)))
          (or (and (progn (forward-char) (re-search-forward "^(" nil t))
                   (progn (backward-char) t))
              (gauche-goto-next-top-level))))))
    globals))

;; for internal defines, etc.
(defun gauche-scheme-current-definitions (&optional enclosing-end)
  (let ((defs '())
        (end (or enclosing-end (point-max))))
    (save-excursion
      (while (< (point) end)
        (let ((here (point))
              (new-defs (gauche-parse-read-definitions)))
          (cond
           (new-defs
            (setq defs (append new-defs defs))
            (or (ignore-errors 
                  (gauche-beginning-of-next-sexp)
                  (> (point) here))
                (goto-char end)))
           ;; non-definition form, maybe stop scanning
           ((not *gauche-interleave-definitions-p*)
            (goto-char end))))))
    defs))

(defun gauche-scheme-srfi-exports (i)
  (and (integerp i)
       (>= i 0)
       (< i (length *gauche-scheme-srfi-info*))
       (let ((info (cdr (aref *gauche-scheme-srfi-info* i))))
         (if (and (consp info) (null (cdr info)) (symbolp (car info)))
             (gauche-scheme-module-exports (car info))
           info))))

(defun gauche-scheme-module-exports (mod)
  ;; TODO consider recursive
  (unless (member mod gauche-processing-modules)
    (push mod gauche-processing-modules))
  (cond
   ((and (consp mod) (eq 'srfi (car mod)))
    (gauche-append-map 'gauche-scheme-srfi-exports (cdr mod)))
   ((and (symbolp mod) (string-match "^srfi-" (symbol-name mod)))
    (gauche-scheme-srfi-exports
     (string-to-number (substring (symbol-name mod) 5))))
   (t
    (let ((cached (assq mod gauche-cache-module-exports)))
      ;; remove stale caches
      (when (and cached
                 (stringp (nth 1 cached))
                 (ignore-errors
                   (let ((mtime (nth 5 (file-attributes (nth 1 cached))))
                         (ptime (nth 2 cached)))
		     (not (or (equal mtime ptime)
			      (time-less-p mtime ptime))))))
        (setq gauche-cache-module-exports
              (assq-delete-all mod gauche-cache-module-exports))
        (setq cached nil))
      (if cached
          (nth 3 cached)
        ;; (re)compute module exports
	(let ((res (gauche-parse-module-exports mod)))
	  (when res
	    (when (and gauche-complete-cache-p (nth 0 res))
	      (push (list mod
			  (nth 0 res)
			  (nth 5 (file-attributes (nth 0 res)))
			  (nth 1 res))
		    gauche-cache-module-exports))
	    (nth 1 res))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For cygwin path 

(defcustom gauche-cygwin-cygdrive "/cygdrive"
  "*Path alias of Windows drive prefixed path in Cygwin.

c:/Windows == /cygdrive/c/Windows
d:/home == /cygdrive/d/home

")

(defcustom gauche-cygwin-directory "c:/cygwin/"
  "*Cygwin installed directory.")

(defun gauche-cygwin-path-to-unix-path (path)
  (let ((prefix gauche-cygwin-cygdrive)
	(installed gauche-cygwin-directory))
    (cond
     ((string-match (format "^\\(%s\\)/\\([a-zA-Z]\\)/\\(.*\\)" (regexp-quote prefix)) path)
      (format "%s:/%s" (match-string 2 path) (match-string 3 path)))
     ((string-match "^/" path)
      (format "%s/%s" (expand-file-name installed) (substring path 1)))
     (t
      path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is rather complicated because we want to auto-generate
;; docstring summaries from the type information, which means
;; inferring various types from common names.  The benefit is that you
;; don't have to input the same information twice, and can often
;; cut&paste&munge procedure descriptions from the original
;; documentation.

(defun gauche-scheme-translate-type (type)
  (if (not (symbolp type))
      type
    (case type
      ((pred proc thunk handler dispatch producer consumer f fn g kons)
       'procedure)
      ((num) 'number)
      ((z) 'complex)
      ((x1 x2 x3 y timeout seconds nanoseconds) 'real)
      ((i j k n m int index size count len length bound nchars start end
          pid uid gid fd fileno errno)
       'integer)
      ((ch) 'char)
      ((str name pattern) 'string)
      ((file path pathname) 'filename)
      ((dir dirname) 'directory)
      ((sym id identifier) 'symbol)
      ((ls lis lst alist lists) 'list)
      ((vec) 'vector)
      ((exc excn err error) 'exception)
      ((ptr) 'pointer)
      ((bool) 'boolean)
      ((env) 'environment)
      ((char string boolean number complex real integer procedure char-set
             port input-port output-port pair list vector array stream hash-table
             thread mutex condition-variable time exception date duration locative
             random-source state condition condition-type queue pointer
             u8vector s8vector u16vector s16vector u32vector s32vector
             u64vector s64vector f32vector f64vector undefined symbol
             block filename directory mmap listener environment non-procedure
             read-table continuation blob generic method class regexp regmatch
             sys-stat fdset)
       type)
      ((parent seed option mode) 'non-procedure)
      (t
       (let* ((str (symbol-name type))
              (i (string-match "-?[0-9]+$" str)))
         (if i
             (gauche-scheme-translate-type (intern (substring str 0 i)))
           (let ((i (string-match "-\\([^-]+\\)$" str)))
             (if i
                 (gauche-scheme-translate-type (intern (substring str (+ i 1))))
               (if (string-match "\\?$" str)
                   'boolean
                 'object)))))))))

(defun gauche-lookup-type (spec pos)
  (let ((i 1)
        (type nil))
    (while (and (consp spec) (<= i pos))
      (cond
       ((eq :optional (car spec))
        (if (and (= i pos) (consp (cdr spec)))
            (setq type (cadr spec)))
        (setq i (+ pos 1)))
       ((= i pos)
        (setq type (car spec))
        (setq spec nil))
       ((and (consp (cdr spec)) (eq '\.\.\. (cadr spec)))
        (setq type (car spec))
        (setq spec nil)))
      (setq spec (cdr spec))
      (incf i))
    (if type
        (setq type (gauche-scheme-translate-type type)))
    type))

(defun gauche-predicate->type (pred)
  (case pred
    ((even? odd?) 'integer)
    ((char-upper-case? char-lower-case?
                       char-alphabetic? char-numeric? char-whitespace?)
     'char)
    (t
     ;; catch all the `type?' predicates with pattern matching
     ;; ... we could be smarter if the env was passed
     (let ((str (symbol-name pred)))
       (if (string-match "\\?$" str)
           (gauche-scheme-translate-type
            (intern (substring str 0 (- (length str) 1))))
         'object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion

(eval-when (compile load eval)
  (unless (fboundp 'event-matches-key-specifier-p)
    (defalias 'event-matches-key-specifier-p 'eq)))

(unless (fboundp 'read-event)
  (defun read-event ()
    (aref (read-key-sequence nil) 0)))

(unless (fboundp 'event-basic-type)
  (defalias 'event-basic-type 'event-key))

(defun gauche-string-starts-with (pref str)
  (let ((p-len (length pref))
        (s-len (length str)))
    (and (<= p-len s-len)
         (equal pref (substring str 0 p-len)))))

(defun gauche-scheme-do-completion (str coll &optional strs pred)
  (let* ((coll (mapcar (lambda (x)
                         (cond
                          ((symbolp x) (list (symbol-name x)))
                          ((stringp x) (list x))
                          (t x)))
                       coll))
         (completion1 (try-completion str coll pred))
         (completion2 (and strs (try-completion str strs pred)))
         (completion (if (and completion2
                              (or (not completion1)
                                  (< (length completion2)
                                     (length completion1))))
                         completion2
                       completion1)))
    (cond
     ((eq completion t))
     ((not completion)
      (message "Can't find completion for \"%s\"" str)
      (ding))
     ((not (string= str completion))
      (let ((prefix-p (gauche-string-starts-with completion completion1)))
        (unless prefix-p
          (save-excursion
            (backward-char (length str))
            (insert "\"")))
        (insert (substring completion (length str)))
        (unless prefix-p
          (insert "\"")
          (backward-char))))
     (t
      (let ((win-config (current-window-configuration))
            (done nil))
        (message "Hit space to flush")
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list
           (sort
            (all-completions str (append strs coll) pred)
            'string-lessp)))
        (while (not done)
          (let* ((orig-event
                  (with-current-buffer (get-buffer "*Completions*")
                    (read-event)))
                 (event (event-basic-type orig-event)))
            (cond
             ((or (event-matches-key-specifier-p event 'tab)
                  (event-matches-key-specifier-p event 9))
              (save-selected-window
                (select-window (get-buffer-window "*Completions*"))
                (if (pos-visible-in-window-p (point-max))
                    (goto-char (point-min))
                  (scroll-up))))
             (t
              (set-window-configuration win-config)
              (if (or (event-matches-key-specifier-p event 'space)
                      (event-matches-key-specifier-p event 32))
                  (bury-buffer (get-buffer "*Completions*"))
                (setq unread-command-events (list orig-event)))
              (setq done t))))))
      ))))

(defun gauche-env-lookup (env sym)
  (let ((spec nil)
        (ls env)
        tmp)
    ;; find most detailed info
    (while ls
      (setq tmp (assq sym (pop ls)))
      (when (and (listp tmp) 
                 (> (length tmp) (length spec)))
        (setq spec tmp)))
    spec))

(defun gauche-inside-module-p ()
  (save-excursion
    (ignore-errors
      (let ((here (point))
            res)
        (goto-char (point-min))
        (while (< (point) here)
          (if (not (re-search-forward "^(\\(?:module\\|library\\)\\s-"))
              (goto-char (point-max))
            (beginning-of-line)
            (let ((mod-point (point)))
              (if (ignore-errors (forward-sexp) t)
                  (if (and (<= mod-point here) (<= here (point)))
                      (setq res t))
                (setq res (<= mod-point here))
                (goto-char (point-max))))))
        res))))

(defun gauche-scheme-current-env ()
  (let ((in-mod-p (gauche-inside-module-p)))
    ;; r5rs
    (let ((env (list *gauche-scheme-r5rs-info*)))
      ;; base language
      (let ((base *gauche-documented-exports*))
        (if (and base (not in-mod-p)) (push base env)))
      ;; imports
      (let ((imports (ignore-errors (gauche-parse-current-imports))))
        (if imports (push imports env)))
      ;; top-level defs
      (let ((top (ignore-errors (gauche-parse-current-globals))))
        (if top (push top env)))
      ;; current local vars
      (let ((locals (ignore-errors (gauche-parse-current-local-vars env))))
        (if locals (push locals env)))
      env)))

;; checking return values:
;;   a should be capable of returning instances of b
(defun gauche-type-match-p (a b)
  (let ((a1 (gauche-scheme-translate-type a))
        (b1 (gauche-scheme-translate-type b)))
    (and (not (eq a1 'undefined))   ; check a *does* return something
         (or (eq a1 b1)             ; and they're the same
             (eq a1 'object)        ; ... or a can return anything
             (eq b1 'object)        ; ... or b can receive anything
             (if (symbolp a1)
                 (if (symbolp b1)
                     (case a1           ; ... or the types overlap
                       ((number complex real rational integer)
                        (memq b1 '(number complex real rational integer)))
                       ((port input-port output-port)
                        (memq b1 '(port input-port output-port)))
                       ((pair list)
                        (memq b1 '(pair list)))
                       ((non-procedure)
                        (not (eq 'procedure b1))))
                   (and
                    (consp b1)
                    (if (eq 'or (car b1))
                        ;; type unions
                        (find-if
                         (lambda (x)
                           (gauche-type-match-p
                            a1 (gauche-scheme-translate-type x)))
                         (cdr b1))
                      (let ((b2 (gauche-translate-special-type b1)))
                        (and (not (equal b1 b2))
                             (gauche-type-match-p a1 b2))))))
               (and (consp a1)
                    (case (car a1)
                      ((or)
                       ;; type unions
                       (find-if
                        (lambda (x)
                          (gauche-type-match-p (gauche-scheme-translate-type x) b1))
                        (cdr a1)))
                      ((lambda)
                       ;; procedures
                       (or (eq 'procedure b1)
                           (and (consp b1)
                                (eq 'lambda (car b1))
                                (gauche-param-list-match-p (cadr a1)
                                                        (cadr b1)))))
                      (t
                       ;; other special types
                       (let ((a2 (gauche-translate-special-type a1))
                             (b2 (gauche-translate-special-type b1)))
                         (and (or (not (equal a1 a2)) (not (equal b1 b2)))
                              (gauche-type-match-p a2 b2)))))))))))

(defun gauche-param-list-match-p (p1 p2)
  (or (and (symbolp p1) (not (null p1)))
      (and (symbolp p2) (not (null p2)))
      (and (null p1) (null p2))
      (and (consp p1) (consp p2)
           (gauche-param-list-match-p (cdr p1) (cdr p2)))))

(defun gauche-translate-special-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((list string) (car x))
      ((set special) (cadr x))
      ((flags) 'integer)
      (t x))))

;;TODO??? nth?
(defun gauche-nth* (n ls)
  (while (and (consp ls) (> n 0))
    (setq n (- n 1)
          ls (cdr ls)))
  (and (consp ls) (car ls)))

(defun gauche-file->lines (file)
  (and (file-readable-p file)
       (gauche-with-find-file file
         (goto-char (point-min))
         (let ((res '()))
           (while (not (eobp))
             (let ((start (point)))
               (forward-line)
               (push (buffer-substring-no-properties start (- (point) 1))
                     res)))
           (reverse res)))))

(defun gauche-complete-file-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir "."))))
    (if dir
        (mapcar (lambda (f) (concat dir f)) res)
      res)))

(defun gauche-complete-directory-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir ".")))
         (res2 (if dir (mapcar (lambda (f) (concat dir f)) res) res)))
    (remove-if-not 'file-directory-p res2)))

(defun gauche-string-completer (type)
  (case type
    ((filename)
     '(gauche-complete-file-name file-name-nondirectory))
    ((directory)
     '(gauche-complete-directory-name file-name-nondirectory))
    (t
     (cond
      ((and (consp type) (eq 'string (car type)))
       (cadr type))
      ((and (consp type) (eq 'or (car type)))
       (car (delete nil (mapcar 'gauche-string-completer (cdr type)))))))))

(defun gauche-apply-string-completer (cmpl sym)
  (let ((func (if (consp cmpl) (car cmpl) cmpl))
        (trans (and (consp cmpl) (cadr cmpl))))
    (funcall func trans sym)))

(defun gauche-smart-complete (&optional arg)
  (interactive "P")
  (let* ((end (point))
         (start (save-excursion (skip-syntax-backward "w_") (point)))
         (sym (buffer-substring-no-properties start end))
         (in-str-p (gauche-in-string-p))
         (x (save-excursion
              (if in-str-p (gauche-beginning-of-string))
              (gauche-enclosing-2-sexp-prefixes)))
         (inner-proc (car x))
         (inner-pos (cadr x))
         (outer-proc (caddr x))
         (outer-pos (cadddr x))
         (env (save-excursion
                (if in-str-p (gauche-beginning-of-string))
                (gauche-scheme-current-env)))
         (outer-spec (gauche-env-lookup env outer-proc))
         (outer-type (gauche-scheme-translate-type (cadr outer-spec)))
         (inner-spec (gauche-env-lookup env inner-proc))
         (inner-type (gauche-scheme-translate-type (cadr inner-spec))))
    (cond
     ;; return all env symbols when a prefix arg is given
     (arg
      (gauche-scheme-do-completion sym (gauche-filter (lambda (x) t) env)))
     ;; allow different types of strings
     (in-str-p
      (let* ((param-type
              (and (consp inner-type)
                   (eq 'lambda (car inner-type))
                   (gauche-lookup-type (cadr inner-type) inner-pos)))
             (completer (or (gauche-string-completer param-type)
                            '(gauche-complete-file-name
                              file-name-nondirectory))))
        (gauche-scheme-do-completion
         ;;(if (consp completer) (funcall (cadr completer) sym) sym)
         sym
         (gauche-apply-string-completer completer sym))))
     ;; outer special
     ((and (consp outer-type)
           (eq 'special (car outer-type))
           (cadddr outer-type))
      (gauche-scheme-do-completion sym (funcall (cadddr outer-type) sym)))
     ;; inner special
     ((and (consp inner-type)
           (eq 'special (car inner-type))
           (caddr inner-type))
      (gauche-scheme-do-completion sym (funcall (caddr inner-type) sym)))
     ;; completing inner procedure, complete procedures with a
     ;; matching return type
     ((and (consp outer-type)
           (eq 'lambda (car outer-type))
           (not (zerop outer-pos))
           (gauche-nth* (- outer-pos 1) (cadr outer-type))
           (or (zerop inner-pos)
               (and (>= 1 inner-pos)
                    (consp inner-type)
                    (eq 'lambda (car inner-type))
                    (let ((param-type
                           (gauche-lookup-type (cadr inner-type) inner-pos)))
                      (and (consp param-type)
                           (eq 'lambda (car param-type))
                           (eq (caddr inner-type) (caddr param-type)))))))
      (let ((want-type (gauche-lookup-type (cadr outer-type) outer-pos)))
        (gauche-scheme-do-completion
         sym
         (gauche-filter
          (lambda (x)
            (let ((type (cadr x)))
              (or (memq type '(procedure object nil))
                  (and (consp type)
                       (or (and (eq 'syntax (car type))
                                (not (eq 'undefined (caddr type))))
                           (and (eq 'lambda (car type))
                                (gauche-type-match-p (caddr type)
                                                  want-type)))))))
          env))))
     ;; completing a normal parameter
     ((and inner-proc
           (not (zerop inner-pos))
           (consp inner-type)
           (eq 'lambda (car inner-type)))
      (let* ((param-type (gauche-lookup-type (cadr inner-type) inner-pos))
             (set-or-flags
              (or (and (consp param-type)
                       (case (car param-type)
                         ((set) (cddr param-type))
                         ((flags) (cdr param-type))))
                  ;; handle nested arithmetic functions inside a flags
                  ;; parameter
                  (and (not (zerop outer-pos))
                       (consp outer-type)
                       (eq 'lambda (car outer-type))
                       (let ((outer-param-type
                              (gauche-lookup-type (cadr outer-type)
                                               outer-pos)))
                         (and (consp outer-param-type)
                              (eq 'flags (car outer-param-type))
                              (memq (gauche-scheme-translate-type param-type)
                                    '(number complex real rational integer))
                              (memq (gauche-scheme-translate-type (caddr inner-type))
                                    '(number complex real rational integer))
                              (cdr outer-param-type))))))
             (base-type (if set-or-flags
                            (if (and (consp param-type)
                                     (eq 'set (car param-type)))
                                (gauche-scheme-translate-type (cadr param-type))
                              'integer)
                          param-type))
             (base-completions
              (gauche-filter
               (lambda (x)
                 (and (not (and (consp (cadr x)) (eq 'syntax (caadr x))))
                      (gauche-type-match-p (cadr x) base-type)))
               env))
             (str-completions
              (let ((completer (gauche-string-completer base-type)))
                (and
                 completer
                 (gauche-apply-string-completer completer sym)))))
        (gauche-scheme-do-completion
         sym
         (append set-or-flags base-completions)
         str-completions)))
     ;; completing a function
     ((zerop inner-pos)
      (gauche-scheme-do-completion
       sym
       (gauche-filter
        (lambda (x)
          (or (null (cdr x))
              (memq (cadr x) '(procedure object nil))
              (and (consp (cadr x))
                   (memq (caadr x) (lambda syntax special)))))
        env)))
     ;; complete everything
     (t
      (gauche-scheme-do-completion sym (gauche-filter (lambda (x) t) env))))))

(defun gauche-complete-or-indent (&optional arg)
  (interactive "P")
  (let* ((end (point))
         (func
          (save-excursion
            (beginning-of-line)
            (if (re-search-forward "\\S-" end t)
                'gauche-smart-complete
              'lisp-indent-line))))
    (funcall func arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional indentation handling

(defvar calculate-lisp-indent-last-sexp)

;; Copied from scheme-indent-function, but ignore
;; scheme-indent-function property for local variables.
(defun gauche-smart-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let* ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             (function-sym (intern-soft function))
             (method (and (not (assq function-sym (gauche-parse-current-local-vars)))
                          (get function-sym 'scheme-indent-function))))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

(defun gauche-object-to-string (obj)
  ;; avoid `max-lisp-eval-depth' error
  (condition-case err
      (with-output-to-string
	(princ (gauche-elisp-to-scheme-string obj)))
    (error "")))

(defun gauche-elisp-to-scheme-string (obj)
  (cond
   ((eq obj nil)
    "()")
   ((atom obj)
    obj)
   ((eq (car obj) 'quote)
    (let (ret)
      (mapcar
       (lambda (x)
	 (setq ret (cons (gauche-elisp-to-scheme-string x) ret)))
       (cdr obj))
      (mapconcat 'gauche-object-to-string ret " ")))
   (t
    (mapcar
     (lambda (x)
       (gauche-elisp-to-scheme-string x))
     obj))))



(defadvice scheme-send-last-sexp
  (around gauche-send-last-sexp-like-elisp () activate)
  (interactive)
  (let ((proc (scheme-get-process))
	buffer results start end)
    (if (null proc)
	(save-window-excursion
	  (scheme-proc)
	  ;; FIXME cannot handle one time. exit...
	  (message "Starting process... Try again"))
      (setq buffer (process-buffer proc))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (setq start (point-max))))
      (setq ad-return-value ad-do-it)
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (while (= start (point-max))
	    (sit-for 0.5))
	  ;; wait return prompt from process.
	  (while (save-excursion
		   (goto-char (point-max))
		   (forward-line 0)
		   (prog1
		       (not (looking-at (concat comint-prompt-regexp "\\'")))
		     (setq end (1- (match-beginning 0)))))
	    (sit-for 0.5))
	  (setq results (buffer-substring start end))))
      (message "%s" results))))

(defun gauche-ac-initialize ()
  (add-to-list 'ac-sources 'ac-source-gauche-symbols)
  (add-to-list 'ac-modes 'gauche-mode))

(defun gauche-ac-candidates ()
  (mapcar
   (lambda (c)
     (cond
      ((symbolp (car c))
       (symbol-name (car c)))))
   (gauche-parse-current-imports)))

(when (require 'auto-complete-config nil t)

  (ac-config-default)

  (ac-define-source gauche-symbols
    '((candidates . gauche-ac-candidates)
      (symbol . "f")
      (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
      (cache)))

  (add-hook 'gauche-mode-hook 'gauche-ac-initialize))

;; TODO
;; when buffer starts with #!/usr/local/gauche-current/bin/gosh
;; when buffer is /usr/local/gauche-current/share/lib/* filename
(defvar gauche-buffer-executable nil)

(defvar gauche-current-executable nil)
(make-variable-buffer-local 'gauche-current-executable)

(defun gauche-current-executable ()
  (or gauche-current-executable
      (setq gauche-current-executable
            (or (save-excursion
                  (goto-char (point-min))
                  (when (looking-at auto-mode-interpreter-regexp)
                    (let ((command (match-string-no-properties 2)))
                      (match-string-no-properties 2))))
            gauche-default-command))))



(require 'info-look)

(defvar gauche-info-appendixes
  '(
    ("(gauche-refj.info)Index - 手続きと構文索引" nil 
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refj.info)Index - モジュール索引"   nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refj.info)Index - クラス索引"      nil
     "^[ \t]+-- [^:]+:[ \t]*" nil)
    ("(gauche-refj.info)Index - 変数索引"        nil
     "^[ \t]+-- [^:]+:[ \t]*" nil))
  ;; '(("(gauche-refe.info)Function and Syntax Index" nil 
  ;;    "^[ \t]+-- [^:]+:[ \t]*" nil)
  ;;   ("(gauche-refe.info)Module Index"   nil
  ;;    "^[ \t]+-- [^:]+:[ \t]*" nil)
  ;;   ("(gauche-refe.info)Class Index"      nil
  ;;    "^[ \t]+-- [^:]+:[ \t]*" nil)
  ;;   ("(gauche-refe.info)Variable Index"        nil
  ;;    "^[ \t]+-- [^:]+:[ \t]*" nil)
  ;;   )
  )

(info-lookup-add-help
 ;; For
 ;;  info-complete-symbol (to complete a symbol using the info)
 ;;  info-lookup-symbol   (to look up a symbol in the info)
 :topic 'symbol
 :mode  'gauche-mode
 :regexp "[^()'\" \t\n]+"
 :ignore-case nil
 :doc-spec `(,@gauche-info-appendixes)
 :parse-rule  nil
 :other-modes nil)



(gauche-default-initialize)



(provide 'gauche-mode)

;;; gauche-mode.el ends here
