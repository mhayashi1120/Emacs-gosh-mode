;;; scm-env.el --- Programming language scheme basic utilities

;; TODO forked version of scheme-complete.el
;; separate chicken and mzscheme


;;; Commentary:
;; 

(eval-when-compile
  (require 'cl))

(require 'scm-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special lookups (XXXX add more impls, try to abstract better)

;;; Code:

(defvar *scm-chicken-base-repo*
  (or (getenv "CHICKEN_REPOSITORY")
      (let ((dir
             (car (remove-if-not #'file-directory-p
                                 '("/usr/lib/chicken"
                                   "/usr/local/lib/chicken"
                                   "/opt/lib/chicken"
                                   "/opt/local/lib/chicken"
                                   )))))
        (and dir
             (car (reverse (sort (directory-files dir t "^[0-9]+$")
                                 #'string-lessp)))))
      (and (fboundp 'shell-command-to-string)
           (let* ((res (shell-command-to-string
                        "csi -e '(print (repository-path))'"))
                  (res (substring res 0 (- (length res) 1))))
             (and res (file-directory-p res) res)))
      "/usr/local/lib/chicken"))

(defvar *scm-chicken-repo-dirs*
  (remove-if-not
   #'(lambda (x) (and (stringp x) (not (equal x ""))))
   (let ((home (getenv "CHICKEN_HOME")))
     (if (and home (not (equal home "")))
         (let ((res (split-string home ";"))) ;
           (if (member *scm-chicken-base-repo* res)
               res
             (cons *scm-chicken-repo-dirs* res))) 
       (list *scm-chicken-base-repo*)))))

(defun scm-chicken-available-modules (&optional sym)
  (append
   (mapcar #'symbol-name (mapcar #'car *scm-chicken-modules*))
   (mapcar
    #'(lambda (f)
        (let ((f (file-name-sans-extension f)))
          (if (equalp "import" (file-name-extension f))
              (file-name-sans-extension f)
            f)))
    (directory-files "." nil "^[^.].*\\.scm$" t))
   (scm-append-map
    #'(lambda (dir)
        (mapcar
         #'(lambda (f)
             (let ((f (file-name-sans-extension f)))
               (if (equalp "import" (file-name-extension f))
                   (file-name-sans-extension f)
                 f)))
         (if (string-match "/4" dir)
             (directory-files dir nil "^[^.].*\\.import\\.\\(so\\|scm\\)$" t)
           (directory-files dir nil "^[^.].*\\.\\(so\\|scm\\)$" t))))
    *scm-chicken-repo-dirs*)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defun scm-append-map (proc init-ls)
  (if (null init-ls)
      '()
    (let* ((ls (reverse init-ls))
           (res (funcall proc (pop ls))))
      (while (consp ls)
        (setq res (append (funcall proc (pop ls)) res)))
      res)))

(defun scm-flatten (ls)
  (cond
   ((consp ls) (cons (car ls) (scm-flatten (cdr ls))))
   ((null ls) '())
   (t (list ls))))

(defun scm-in-string-p ()
  (let ((orig (point)))
    (save-excursion
      (goto-char (point-min))
      (let ((parses (parse-partial-sexp (point) orig)))
        (nth 3 parses)))))

(defun scm-beginning-of-sexp ()
  (unless (bobp)
    (let ((syn (char-syntax (char-before (point)))))
      (if (or (eq syn ?\()
              (and (eq syn ?\") (scm-in-string-p)))
          (forward-char -1)
        (forward-sexp -1)))))

(defun scm-find-file-in-path (file path)
  (car (remove-if-not
        #'(lambda (dir) (file-exists-p (concat dir "/" file)))
        path)))

(defmacro scm-with-find-file (path-expr &rest body)
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

(defun scm-directory-tree-files (init-dir &optional full match)
  (let ((res '())
        (stack (list init-dir)))
    (while (consp stack)
      (let* ((dir (pop stack))
             (files (cddr (directory-files dir full))))
        (setq res (append (if match
                              (remove-if-not
                               #'(lambda (f) (string-match match f))
                               files)
                            files)
                          res))
        (setq stack
              (append
               (remove-if-not 'file-directory-p
                              (if full
                                  files
                                (mapcar #'(lambda (f) (concat dir "/" f))
                                        files)))
               stack))))
    res))

(defmacro scm-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'scm-aif 'lisp-indent-function 2)

(put 'scm-case-defun 'lisp-indent-function 2)
(defmacro scm-case-defun (name arglist &rest body)
  "Define following definitions.
(defun NAME arglist)
(defun NAME-generic arglist)
(defvar NAME-function)
(defvar NAME-functions)

\(fn NAME ARGLIST [DOCSTRING] &rest BODY)"
  (let ((main-name name)
	(generic-name (intern (concat (symbol-name name) "-generic")))
	(func-var (intern (concat (symbol-name name) "-function")))
	(funcs-var (intern (concat (symbol-name name) "-functions"))))
    `(progn
       (defun ,main-name ,arglist
	 ,(when (stringp (car body))
	    (car body))
	 (let ((fun
		(or ,func-var
		    (cdr (assq (scm-current-implementation)
			       ,funcs-var)))))
	   (if fun
	       (funcall fun ,@arglist)
	     (,generic-name ,@arglist))))
       (defun ,generic-name ,arglist
	 ,@body)
       (defvar ,func-var nil)
       (make-variable-buffer-local ',func-var)
       (defvar ,funcs-var nil)
       ',main-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alist utilities

(defun scm-put-alist (key value alist)
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

(defun scm-set-alist (symbol key value)
  "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE.

This function come from apel"
  (or (boundp symbol)
      (set symbol nil))
  (set symbol (scm-put-alist key value (symbol-value symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sexp manipulation

;; returns current argument position within sexp
(defun scm-beginning-of-current-sexp-operator ()
  (let ((pos 0))
    (skip-syntax-backward "w_")
    (while (and (not (bobp)) (not (eq ?\( (char-before))))
      (scm-beginning-of-sexp)
      (incf pos))
    pos))

(defun scm-beginning-of-next-sexp ()
  (forward-sexp 2)
  (backward-sexp 1))

(defun scm-beginning-of-string ()
  (interactive)
  (search-backward "\"" nil t)
  (while (and (> (point) (point-min)) (eq ?\\ (char-before)))
    (search-backward "\"" nil t)))

;; for the enclosing sexp, returns a cons of the leading symbol (if
;; any) and the current position within the sexp (starting at 0)
;; (defun scm-enclosing-sexp-prefix ()
;;   (save-excursion
;;     (let ((pos (scm-beginning-of-current-sexp-operator)))
;;       (cons (scm-symbol-at-point) pos))))

(defun scm-enclosing-2-sexp-prefixes ()
  (save-excursion
    (let* ((pos1 (scm-beginning-of-current-sexp-operator))
           (sym1 (scm-symbol-at-point)))
      (backward-char)
      (or
       (ignore-errors
         (let ((pos2 (scm-beginning-of-current-sexp-operator)))
           (list sym1 pos1 (scm-symbol-at-point) pos2)))
       (list sym1 pos1 nil 0)))))

;; sexp-at-point is always fragile, both because the user can input
;; incomplete sexps and because some scheme sexps are not valid elisp
;; sexps.  this is one of the few places we use it, so we're careful
;; to wrap it in ignore-errors.
(defun scm-nth-sexp-at-point (n)
  (ignore-errors
    (save-excursion
      (forward-sexp (+ n 1))
      (let ((end (point)))
        (forward-sexp -1)
        (car (scm-read-from-string (buffer-substring (point) end)))))))

(defun scm-read-from-string (string)
  (condition-case err
      (read-from-string string)
    (invalid-read-syntax 
     (if (string= (cadr err) "#")
	 ;;FIXME not concern about in string?
	 ;; Probablly allmost case is ok.
	 (read-from-string (replace-regexp-in-string "#" "\\\\#" string))
       (error "Assert")))))

(defun scm-symbol-at-point ()
  (save-excursion
    (skip-syntax-backward "w_")
    (let ((start (point)))
      (skip-syntax-forward "w_")
      (and (< start (point))
           (intern (buffer-substring start (point)))))))

(defun scm-goto-next-top-level ()
  (let ((here (point)))
    (or (ignore-errors (end-of-defun) (end-of-defun)
                       (beginning-of-defun)
                       (< here (point)))
        (progn (forward-char)
               (and (re-search-forward "^(" nil t)
                    (progn (backward-char 1) t)))
        (goto-char (point-max)))))

(defun scm-keyword-at-point ()
  (let ((sym (scm-symbol-at-point)))
    (when (and sym
	       (keywordp sym))
      sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable extraction

(defun scm-sexp-type-at-point (&optional env)
  (case (char-syntax (char-after))
    ((?\()
     (forward-char 1)
     (if (eq ?w (char-syntax (char-after)))
         (let ((op (scm-symbol-at-point)))
           (cond
            ((eq op 'lambda)
             (let ((params
                    (scm-nth-sexp-at-point 1)))
               `(lambda ,params)))
            (t
             (let ((spec (scm-env-lookup env op)))
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

(defun scm-let-vars-at-point (&optional env limit loopp)
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
              (scm-beginning-of-next-sexp))
          (if (eq ?w (char-syntax (char-after)))
              (let* ((sym (scm-symbol-at-point))
                     (type (and (not loopp)
                                (ignore-errors
                                  (scm-beginning-of-next-sexp)
                                  (scm-sexp-type-at-point env)))))
                (push (if type (list sym type) (list sym)) vars)))
          (when loopp
            (while (and (< (point) end)
                        (ignore-errors
                          (scm-beginning-of-next-sexp)
                          (eq ?w (char-syntax (char-after)))))
              (push (list (scm-symbol-at-point)) vars)))))
      (unless (ignore-errors (let ((here (point)))
                               (scm-beginning-of-next-sexp)
                               (> (point) here)))
        (goto-char end)))
    (reverse vars)))

(defun scm-extract-match-clause-vars (x)
  (cond
   ((null x) '())
   ((symbolp x)
    (if (memq x '(_ ___ \.\.\.))
        '()
      (list (list x))))
   ((consp x)
    (case (car x)
      ((or not)
       (scm-extract-match-clause-vars (cdr x)))
      ((and)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (consp (caddr x))
                (not (memq (caaddr x)
                           '(= $ @ ? and or not quote quasiquote get! set!))))
           (cons (list (cadr x) (if (listp (caddr x)) 'list 'pair))
                 (scm-extract-match-clause-vars (cddr x)))
         (scm-extract-match-clause-vars (cddr x))))
      ((= $ @)
       (if (consp (cdr x)) (scm-extract-match-clause-vars (cddr x)) '()))
      ((\? ? ) ; XXXX this is a hack, the lone ? gets read as a char (space)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (symbolp (caddr x)))
           (cons (list (caddr x) (scm-predicate->type (cadr x)))
                 (scm-extract-match-clause-vars (cdddr x)))
         (scm-extract-match-clause-vars (cddr x))))
      ((get! set!)
       (if (consp (cdr x)) (scm-extract-match-clause-vars (cadr x)) '()))
      ((quote) '())
      ((quasiquote) '())                ; XXXX
      (t
       (union (scm-extract-match-clause-vars (car x))
              (scm-extract-match-clause-vars (cdr x))))))
   ((vectorp x)
    (scm-extract-match-clause-vars (concatenate 'list x)))
   (t
    '())))

;; call this from the first opening paren of the match clauses
(defun scm-extract-match-vars (&optional pos limit)
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
            (let* ((pat (scm-nth-sexp-at-point 0))
                   (new-vars (ignore-errors
                               (scm-extract-match-clause-vars pat))))
              (setq match-vars (append new-vars match-vars)))
            (goto-char (if (or pos (not end)) limit (+ end 1)))))))
      match-vars)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can set the *scm-default-implementation* to your preferred
;; implementation, for when we can't figure out the file from
;; heuristics.  Alternately, in any given buffer, just
;;
;; (setq *scm-current-implementation* whatever)

(defgroup scm-complete nil
  "Smart tab completion"
  :group 'scheme)

(defcustom scm-default-implementation nil
  "Default scheme implementation to provide completion for
when scm-complete can't infer the current implementation."
  :type 'symbol
  :group 'scm-complete)

(defcustom scm-complete-smart-indent-p t
  "Toggles using `scm-smart-indent' for `scm-complete-or-indent'."
  :type 'boolean
  :group 'scm-complete)

(defcustom scm-complete-cache-p t
  "Toggles caching of module/load export information."
  :type 'boolean
  :group 'scm-complete)

;; (defcustom scm-complete-learn-syntax-p nil
;;   "Toggles parsing of syntax-rules macros for completion info."
;;   :type 'boolean
;;   :group 'scm-complete)

(defvar *scm-interleave-definitions-p* nil)

(defvar *scm-complete-module-cache* '()
  "Each item is following list
(module-symbol module-file time exported-symbols [defined-symbols])"
  )

(defvar *scm-current-implementation* nil)
(make-variable-buffer-local '*scm-current-implementation*)

;; most implementations use their name as the script name
(defvar *scm-interpreter-alist*
  '(("csi"  . chicken)
    ("gosh" . gauche)
    ("gsi"  . gambit)
    ("mred" . mzscheme)
    ))

(defvar *scm-imported-modules* '())

(defun scm-current-implementation ()
  (or *scm-current-implementation*
      (setq *scm-current-implementation*
	    (or
	     (save-excursion
	       (goto-char (point-min))
	       (or
		(and (looking-at "#! *\\([^ \t\n]+\\)")
		     (let ((script (file-name-nondirectory (match-string 1))))
		       (cdr (assoc script *scm-interpreter-alist*))))
		(cond
		 ((re-search-forward "(define-module +\\(.\\)" nil t)
		  (if (equal "(" (match-string 1))
		      'guile
		    'gauche))
		 ((re-search-forward "(\\(?:use\\|require-library\\) " nil t)
		  'chicken)
		 ((re-search-forward
		   "#\\(?:lang\\|reader\\)" nil t)
		  'mzscheme)
		 ((re-search-forward "(module\\s-" nil t)
		  (if (looking-at "\\s-*\\sw") 'chicken 'mzscheme)))))
	     scm-default-implementation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scm-current-local-vars (&optional env)
  (let ((vars '())
        (start (point))
        (limit (save-excursion (beginning-of-defun) (+ (point) 1)))
        (let-limit (save-excursion (scm-beginning-of-sexp)
                                   (scm-beginning-of-sexp)
                                   (point)))
        (scan-internal))
    (save-excursion
      (while (> (point) limit)
        (or (ignore-errors
              (progn
                (skip-chars-backward " \t\n" limit)
                (scm-beginning-of-sexp)
                t))
            (goto-char limit))
        (when (and (> (point) (point-min))
                   (eq ?\( (char-syntax (char-before (point))))
                   (eq ?w (char-syntax (char-after (point)))))
          (setq scan-internal t)
          (let ((sym (scm-symbol-at-point)))
            (case sym
              ((lambda)
               (setq vars
                     (append
                      (mapcar #'list
                              (scm-flatten (scm-nth-sexp-at-point 1)))
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
                            (scm-extract-match-vars
                             (and (or (eq sym 'match) (< start limit)) start)
                             limit))))
                      vars)))
              ((let let* letrec letrec* let-syntax letrec-syntax
                and-let* do loop)
               (or
                (ignore-errors
                  (save-excursion
                    (scm-beginning-of-next-sexp)
                    (let* ((loop-name
                            (and (memq sym '(let loop))
                                 (eq ?w (char-syntax (char-after (point))))
                                 (prog1 (scm-symbol-at-point)
                                   (scm-beginning-of-next-sexp))))
                           (let-vars
                            (scm-let-vars-at-point
                             env let-limit (eq sym 'loop))))
                      (if loop-name
                          ;; named let
                          (setq vars
                                (cons `(,loop-name (lambda ,(mapcar #'car let-vars)))
                                      (append let-vars vars)))
                        (setq vars (append let-vars vars))))
                    t))
                (goto-char limit)))
              ((let-values let*-values)
               (setq vars
                     (append (mapcar
                              #'list
                              (scm-append-map
                               #'scm-flatten
                               (remove-if-not #'consp
                                              (scm-nth-sexp-at-point 1))))
                             vars)))
              ((receive defun defmacro)
               (setq vars
                     (append (mapcar #'list
                                     (scm-flatten
                                      (scm-nth-sexp-at-point 1)))
                             vars)))
              (t
               (if (string-match "^define\\(-.*\\)?" (symbol-name sym))
                   (let ((defs (save-excursion
                                 (backward-char)
                                 (scm-extract-definitions))))
                     (setq vars
                           (append (scm-append-map
                                    #'(lambda (x)
                                        (and (consp (cdr x))
                                             (consp (cadr x))
                                             (eq 'lambda (caadr x))
                                             (mapcar #'list
                                                     (scm-flatten
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
                      (setq vars (append (scm-current-definitions) vars))
                    ))))))))
    (reverse vars)))

(defun scm-extract-import-module-imports (sexp)
  (case (and (consp sexp) (car sexp))
    ((prefix prefix-in)
     (let* ((ids (scm-extract-import-module-imports (cadr sexp)))
            (prefix0 (caddr sexp))
            (prefix (if (symbolp prefix0) (symbol-name prefix0) prefix0)))
       (mapcar #'(lambda (x)
                   (cons (intern (concat prefix (symbol-name (car x))))
                         (cdr x)))
               ids)))
    ((prefix-all-except)
     (let ((prefix
            (if (symbolp (cadr sexp)) (symbol-name (cadr sexp)) (cadr sexp)))
           (exceptions (cddr sexp)))
       (mapcar #'(lambda (x)
                   (if (memq (car x) exceptions)
                       x
                     (cons (intern (concat prefix (symbol-name (car x))))
                           (cdr x))))
               (scm-extract-import-module-imports (caddr sexp)))))
    ((for for-syntax for-template for-label for-meta)
     (scm-extract-import-module-imports (cadr sexp)))
    ((rename rename-in)
     (let ((renames (cddr sexp)))
       (mapcar #'(lambda (x)
                   (cons (or (cadr (assq (car x) renames)) (car x)) (cdr x)))
               (scm-extract-import-module-imports (cadr sexp)))))
    ((except except-in)
     (remove-if #'(lambda (x) (memq (car x) (cddr sexp)))
                (scm-extract-import-module-imports (cadr sexp))))
    ((only only-in)
     (remove-if-not
      #'(lambda (x) (memq (car x) (cddr sexp)))
      (scm-extract-import-module-imports (cadr sexp))))
    ((import import-for-syntax require)
     (scm-append-map #'scm-extract-import-module-imports (cdr sexp)))
    ((library)
     (if (and (stringp (cadr sexp)) (file-exists-p (cadr sexp)))
         (scm-module-exports (intern (cadr sexp)))))
    ((lib)
     (if (and (equal "srfi" (caddr sexp))
              (stringp (cadr sexp))
              (string-match "^[0-9]+\\." (cadr sexp)))
         (scm-module-exports
          (intern (file-name-sans-extension (concat "srfi-" (cadr sexp)))))
       (scm-module-exports
        (intern (apply 'concat (append (cddr sexp) (list (cadr sexp))))))))
    (t
     (scm-module-exports sexp))))

(defun scm-extract-sexp-imports (sexp)
  (case (and (consp sexp) (car sexp))
    ((begin define-module)
     (scm-append-map #'scm-extract-sexp-imports (cdr sexp)))
    ((cond-expand)
     (scm-append-map #'scm-extract-sexp-imports
                        (scm-append-map #'cdr (cdr sexp))))
    ((use require-extension)
     (scm-append-map #'scm-module-exports (cdr sexp)))
    ((import)
     (scm-append-map #'scm-extract-import-module-imports (cdr sexp)))
    ((autoload)
     (unless (member (cadr sexp) *scm-imported-modules*)
       (push (cadr sexp) *scm-imported-modules*)
       (mapcar #'(lambda (x) (cons (if (consp x) (car x) x) '((lambda obj))))
               (cddr sexp))))
    ((load)
     (unless (member (cadr sexp) *scm-imported-modules*)
       (push (cadr sexp) *scm-imported-modules*)
       (and (stringp (cadr sexp))
	    ;;TODO gauche accepts relative path in *load-path*
            (file-exists-p (cadr sexp))
            (scm-with-find-file (cadr sexp)
              (scm-current-globals)))))
    ((library module)
     (scm-append-map #'scm-extract-import-module-imports
                        (remove-if #'(lambda (x)
                                       (memq (car x) '(import require)))
                                   (cdr sexp))))
    ))

(defun scm-module-symbol-p (sym)
  (memq sym '(use require require-extension begin cond-expand
              module library define-module autoload load import)))

(defun scm-skip-shebang ()
  ;; skip shebang if present
  (if (looking-at "#!")
      ;; guile skips until a closing !#
      (if (eq 'guile (scm-current-implementation))
          (re-search-forward "!#" nil t)
        (forward-line))))

(defun scm-current-imports ()
  (let ((imports '())
        (*scm-imported-modules* '()))
    (save-excursion
      (goto-char (point-min))
      (scm-skip-shebang)
      ;; scan for module forms
      (while (not (eobp))
        (if (ignore-errors (forward-sexp) t)
            (let ((end (point))
                  (inside-p nil))
              (backward-sexp)
              (when (eq ?\( (char-after))
                (forward-char)
                (when (not (eq ?\( (char-after)))
                  (let ((sym (scm-symbol-at-point)))
                    (cond
                     ((memq sym '(module library))
                      (forward-sexp 3)
                      (setq inside-p t))
                     ((scm-module-symbol-p sym)
                      (backward-char)
                      (ignore-errors
                        (setq imports
                              (append (scm-extract-sexp-imports
                                       (scm-nth-sexp-at-point 0))
                                      imports))))))))
              (unless inside-p (goto-char end)))
          ;; if an incomplete sexp is found, try to recover at the
          ;; next line beginning with an open paren
          (scm-goto-next-top-level))))
    imports))

;; we should be just inside the opening paren of an expression
(defun scm-name-of-define ()
  (save-excursion
    (scm-beginning-of-next-sexp)
    (if (eq ?\( (char-syntax (char-after)))
        (forward-char))
    (and (memq (char-syntax (char-after)) '(?\w ?\_))
         (scm-symbol-at-point))))

(defun scm-type-of-define ()
  (save-excursion
    (scm-beginning-of-next-sexp)
    (cond
     ((eq ?\( (char-syntax (char-after)))
      `(lambda ,(cdr (scm-nth-sexp-at-point 0))))
     (t
      (ignore-errors (scm-beginning-of-next-sexp)
                     (scm-sexp-type-at-point))))))

;; we should be at the opening paren of an expression
(defun scm-extract-definitions (&optional env)
  (save-excursion
    (let ((sym (ignore-errors (and (eq ?\( (char-syntax (char-after)))
                                   (progn (forward-char)
                                          (scm-symbol-at-point))))))
      (case sym
        ((define-syntax define-compiled-syntax defmacro define-macro)
         (list (list (scm-name-of-define) '(syntax))))
        ((define define-inline define-constant define-primitive defun)
         (let ((name (scm-name-of-define))
               (type (scm-type-of-define)))
           (list (if type (list name type) (list name)))))
        ((defvar define-class)
         (list (list (scm-name-of-define) 'non-procedure)))
        ((define-record)
         (backward-char)
         (ignore-errors
           (let* ((sexp (scm-nth-sexp-at-point 0))
                  (name (symbol-name (cadr sexp))))
             `((,(intern (concat name "?")) (lambda (obj) boolean))
               (,(intern (concat "make-" name)) (lambda ,(cddr sexp) ))
               ,@(scm-append-map
                  #'(lambda (x)
                      `((,(intern (concat name "-" (symbol-name x)))
                         (lambda (non-procedure)))
                        (,(intern (concat name "-" (symbol-name x) "-set!"))
                         (lambda (non-procedure val) undefined))))
                  (cddr sexp))))))
        ((define-record-type)
         (backward-char)
         (ignore-errors
           (let ((sexp (scm-nth-sexp-at-point 0)))
             `((,(caaddr sexp) (lambda ,(cdaddr sexp)))
               (,(cadddr sexp) (lambda (obj)))
               ,@(scm-append-map 
                  #'(lambda (x)
                      (if (consp x)
                          (if (consp (cddr x))
                              `((,(cadr x) (lambda (non-procedure)))
                                (,(caddr x)
                                 (lambda (non-procedure val) undefined)))
                            `((,(cadr x) (lambda (non-procedure)))))))
                  (cddddr sexp))))))
        ((begin progn)
         (forward-sexp)
         (scm-current-definitions))
        (t
         '())))))

(defun scm-in-defun-name ()
  (save-excursion
    (dotimes (i 2)
      (scm-beginning-of-sexp)
      (unless (bobp)
        (backward-char)))
    (and (= 0 (current-column))
         (looking-at "(define")
         (point))))

;; a little more liberal than -definitions, we try to scan to a new
;; top-level form (i.e. a line beginning with an open paren) if
;; there's an error during normal sexp movement
(defun scm-current-globals ()
  (let ((here (point))
        (skip (scm-in-defun-name))
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
              (scm-beginning-of-next-sexp))
             (t ;; not inside the module, skip it altogether
              (forward-sexp 1)
              (scm-goto-next-top-level)))))
         (t
          (if (not (eq (point) skip))
              (setq globals
                    (append
                     (ignore-errors (scm-extract-definitions))
                     globals)))
          (or (and (progn (forward-char) (re-search-forward "^(" nil t))
                   (progn (backward-char) t))
              (scm-goto-next-top-level))))))
    globals))

;; for internal defines, etc.
(defun scm-current-definitions (&optional enclosing-end)
  (let ((defs '())
        (end (or enclosing-end (point-max))))
    (save-excursion
      (while (< (point) end)
        (let ((here (point))
              (new-defs (scm-extract-definitions)))
          (cond
           (new-defs
             (setq defs (append new-defs defs))
             (or (ignore-errors (scm-beginning-of-next-sexp)
                                (> (point) here))
                 (goto-char end)))
           ;; non-definition form, maybe stop scanning
           ((not *scm-interleave-definitions-p*)
            (goto-char end))))))
    defs))

(scm-case-defun scm-current-exports ()
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
		 (cond
		  ((and (listp decls) (assq 'export decls))
		   (setq res (nconc (cdr (assq 'export decls)) res)))
		  ((and (listp decls) (assq 'export-all decls))
		   (goto-char (point-max))))))
              ((export provide)
               (unless (and (eq 'provide sym)
                            (eq 'chicken (scm-current-implementation)))
                 (setq res (nconc (cdr (scm-nth-sexp-at-point 0)) res))))
              ((export-all)
               (goto-char (point-max)))
              ((extend)
               (let ((parents (cdr (scm-nth-sexp-at-point 0))))
                 (setq res (nconc (mapcar #'car
                                          (scm-append-map
                                           #'scm-module-exports
                                           parents))
                                  res))))
              ((module)
               (forward-char)
               (forward-sexp)
               (let ((x (scm-nth-sexp-at-point 0)))
                 (cond
                  ((eq '* x)
                   (goto-char (point-max)))
                  ((listp x)
                   (setq res
                         (nconc (remove-if-not #'symbolp (cdr x)) res)))))))))
        (scm-goto-next-top-level)))
    res))

(defun scm-srfi-exports (i)
  (and (integerp i)
       (>= i 0)
       (< i (length *scm-srfi-info*))
       (let ((info (cdr (aref *scm-srfi-info* i))))
         (if (and (consp info) (null (cdr info)) (symbolp (car info)))
             (scm-module-exports (car info))
           info))))

(scm-case-defun scm-load-path ()
  '())

(defun scm-module-exports (mod)
  ;; TODO consider recursive
  (unless (member mod *scm-imported-modules*)
    (push mod *scm-imported-modules*))
  (cond
   ((and (consp mod) (eq 'srfi (car mod)))
    (scm-append-map #'scm-srfi-exports (cdr mod)))
   ((and (symbolp mod) (string-match "^srfi-" (symbol-name mod)))
    (scm-srfi-exports
     (string-to-number (substring (symbol-name mod) 5))))
   (t
    (let ((cached (assq mod *scm-complete-module-cache*)))
      ;; remove stale caches
      (when (and cached
                 (stringp (nth 1 cached))
                 (ignore-errors
                   (let ((mtime (nth 5 (file-attributes (nth 1 cached))))
                         (ptime (nth 2 cached)))
		     (not (or (equal mtime ptime)
			      (time-less-p mtime ptime))))))
        (setq *scm-complete-module-cache*
              (assq-delete-all mod *scm-complete-module-cache*))
        (setq cached nil))
      (if cached
          (nth 3 cached)
        ;; (re)compute module exports
	(let ((res (scm-module-exports-internal mod)))
	  (when res
	    (when (and scm-complete-cache-p (nth 0 res))
	      (push (list mod
			  (nth 0 res)
			  (nth 5 (file-attributes (nth 0 res)))
			  (nth 1 res))
		    *scm-complete-module-cache*))
	    (nth 1 res))))))))

(scm-case-defun scm-module-exports-internal (mod)
  nil)

(scm-set-alist 'scm-module-exports-internal-functions 'chicken 'scm-module-exports/chicken)
(scm-set-alist 'scm-module-exports-internal-functions 'mzscheme 'scm-module-exports/mzscheme)

(defun scm-module-exports/chicken (mod)
  (let ((predefined (assq mod *scm-chicken-modules*)))
    (if predefined
        (list nil (cdr predefined))
      (let* ((mod-str (symbol-name mod))
             (export-file
              (concat *scm-chicken-base-repo* "/" mod-str ".exports"))
             (setup-file
              (concat *scm-chicken-base-repo* "/" mod-str ".setup-info"))
             ;; look for the source in the current directory
             (source-file (concat mod-str ".scm"))
             ;; try the chicken 4 modules db
             (modules-db (concat *scm-chicken-base-repo* "/modules.db")))
        (cond
         ((eq mod 'scheme)
          (list nil *scm-r5rs-info*))
         ((file-exists-p source-file)
          (list source-file
                (scm-with-find-file source-file
                  (let ((env (scm-current-globals))
                        (exports (scm-current-exports)))
                    (if (consp exports)
                        (remove-if-not #'(lambda (x) (memq (car x) exports)) env)
                      env)))))
         ((file-exists-p export-file)
          (list export-file
                (mapcar #'(lambda (x) (cons (intern x) '((lambda obj))))
                        (scm-file->lines export-file))))
         (t
          (let ((setup-file-exports
                 (and (file-exists-p setup-file)
                      (scm-with-find-file setup-file
                        (let* ((alist (scm-nth-sexp-at-point 0))
                               (cell (assq 'exports alist)))
                          (cdr cell))))))
            (cond
             (setup-file-exports
              (list setup-file
                    (mapcar #'(lambda (x) (cons (intern x) '((lambda obj))))
                            setup-file-exports)))
             ((file-exists-p modules-db)
              (list modules-db
                    (mapcar
                     #'(lambda (x)
                         (cons (intern (car (split-string (substring x 1))))
                               '((lambda ()))))
                     (remove-if-not
                      #'(lambda (x) (string-match (concat " " mod-str ")") x))
                      (scm-file->lines modules-db))))))))
         )))))

(defun scm-module-exports/mzscheme (mod)
  (let ((dir (scm-find-file-in-path
              (symbol-name mod)
              '("."
                "/usr/local/lib/plt/collects"
                "/usr/local/lib/plt/collects/mzlib"))))
    (when dir
      ;; XXXX parse, don't use regexps
      (list
       (concat dir "/" (symbol-name mod))
       (scm-with-find-file (concat dir "/" (symbol-name mod))
         (when (re-search-forward "(provide" nil t)
           (backward-sexp)
           (backward-char)
           (mapcar #'list (cdr (ignore-errors (scm-nth-sexp-at-point 0))))
           ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For cygwin path 

(defcustom scm-cygwin-cygdrive "/cygdrive"
  "*Path alias of Windows drive prefixed path in Cygwin.

c:/Windows == /cygdrive/c/Windows
d:/home == /cygdrive/d/home

")

(defcustom scm-cygwin-directory "c:/cygwin/"
  "*Cygwin installed directory.")

(defun scm-cygwin-path-to-unix-path (path)
  (let ((prefix scm-cygwin-cygdrive)
	(installed scm-cygwin-directory))
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

(defun scm-translate-type (type)
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
             (scm-translate-type (intern (substring str 0 i)))
           (let ((i (string-match "-\\([^-]+\\)$" str)))
             (if i
                 (scm-translate-type (intern (substring str (+ i 1))))
               (if (string-match "\\?$" str)
                   'boolean
                 'object)))))))))

(defun scm-lookup-type (spec pos)
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
        (setq type (scm-translate-type type)))
    type))

(defun scm-predicate->type (pred)
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
           (scm-translate-type
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

(defun scm-string-prefix-p (pref str)
  (let ((p-len (length pref))
        (s-len (length str)))
    (and (<= p-len s-len)
         (equal pref (substring str 0 p-len)))))

(defun scm-do-completion (str coll &optional strs pred)
  (let* ((coll (mapcar #'(lambda (x)
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
      (let ((prefix-p (scm-string-prefix-p completion completion1)))
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

(defun scm-env-lookup (env sym)
  (let ((spec nil)
        (ls env))
    (while (and ls (not spec))
      (setq spec (assq sym (pop ls))))
    spec))

(defun scm-inside-module-p ()
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

(defun scm-current-env ()
  (let ((in-mod-p (scm-inside-module-p)))
    ;; r5rs
    (let ((env (if in-mod-p
                   '(((import
                       (special symbol scm-chicken-available-modules))))
                 (list *scm-r5rs-info*))))
      ;; base language
      (let ((base (cdr (assq (scm-current-implementation)
                             *scm-implementation-exports*))))
        (if (and base (not in-mod-p)) (push base env)))
      ;; imports
      (let ((imports (ignore-errors (scm-current-imports))))
        (if imports (push imports env)))
      ;; top-level defs
      (let ((top (ignore-errors (scm-current-globals))))
        (if top (push top env)))
      ;; current local vars
      (let ((locals (ignore-errors (scm-current-local-vars env))))
        (if locals (push locals env)))
      env)))

(defun scm-env-filter (pred env)
  (mapcar #'car
          (apply #'concatenate
                 'list
                 (mapcar #'(lambda (e) (remove-if-not pred e)) env))))

;; checking return values:
;;   a should be capable of returning instances of b
(defun scm-type-match-p (a b)
  (let ((a1 (scm-translate-type a))
        (b1 (scm-translate-type b)))
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
                         #'(lambda (x)
                             (scm-type-match-p
                              a1 (scm-translate-type x)))
                         (cdr b1))
                      (let ((b2 (scm-translate-special-type b1)))
                        (and (not (equal b1 b2))
                             (scm-type-match-p a1 b2))))))
               (and (consp a1)
                    (case (car a1)
                      ((or)
                       ;; type unions
                       (find-if
                        #'(lambda (x)
                            (scm-type-match-p (scm-translate-type x) b1))
                        (cdr a1)))
                      ((lambda)
                       ;; procedures
                       (or (eq 'procedure b1)
                           (and (consp b1)
                                (eq 'lambda (car b1))
                                (scm-param-list-match-p (cadr a1)
                                                           (cadr b1)))))
                      (t
                       ;; other special types
                       (let ((a2 (scm-translate-special-type a1))
                             (b2 (scm-translate-special-type b1)))
                         (and (or (not (equal a1 a2)) (not (equal b1 b2)))
                              (scm-type-match-p a2 b2)))))))))))

(defun scm-param-list-match-p (p1 p2)
  (or (and (symbolp p1) (not (null p1)))
      (and (symbolp p2) (not (null p2)))
      (and (null p1) (null p2))
      (and (consp p1) (consp p2)
           (scm-param-list-match-p (cdr p1) (cdr p2)))))

(defun scm-translate-special-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((list string) (car x))
      ((set special) (cadr x))
      ((flags) 'integer)
      (t x))))

;;TODO??? nth?
(defun scm-nth* (n ls)
  (while (and (consp ls) (> n 0))
    (setq n (- n 1)
          ls (cdr ls)))
  (and (consp ls) (car ls)))

(defun scm-file->lines (file)
  (and (file-readable-p file)
       (scm-with-find-file file
         (goto-char (point-min))
         (let ((res '()))
           (while (not (eobp))
             (let ((start (point)))
               (forward-line)
               (push (buffer-substring-no-properties start (- (point) 1))
                     res)))
           (reverse res)))))

(defun scm-passwd-file-names (file &optional pat)
  (delete
   nil
   (mapcar
    #'(lambda (line)
        (and (not (string-match "^[ \t]*#" line))
             (or (not pat) (string-match pat line))
             (string-match "^\\([^:]*\\):" line)
             (match-string 1 line)))
    (scm-file->lines file))))

(defun scm-host-file-names (file)
  (scm-append-map
   #'(lambda (line)
       (let ((i (string-match "#" line)))
         (if i (setq line (substring line 0 i))))
       (cdr (split-string line)))
   (scm-file->lines file)))

(defun scm-ssh-known-hosts-file-names (file)
  (scm-append-map
   #'(lambda (line)
       (split-string (car (split-string line)) ","))
   (scm-file->lines file)))

(defun scm-ssh-config-file-names (file)
  (scm-append-map
   #'(lambda (line)
       (and (string-match "^ *Host" line)
            (cdr (split-string line))))
   (scm-file->lines file)))

(defun scm-complete-user-name (trans sym)
  (if (string-match "apple" (emacs-version))
      (append (scm-passwd-file-names "/etc/passwd" "^[^_].*")
              (delete "Shared" (directory-files "/Users" nil "^[^.].*")))
    (scm-passwd-file-names "/etc/passwd")))

(defun scm-complete-host-name (trans sym)
  (append (scm-host-file-names "/etc/hosts")
          (scm-ssh-known-hosts-file-names "~/.ssh/known_hosts")
          (scm-ssh-config-file-names "~/.ssh/config")))

;; my /etc/services is 14k lines, so we try to optimize this
(defun scm-complete-port-name (trans sym)
  (and (file-readable-p "/etc/services")
       (scm-with-find-file "/etc/services"
         (goto-char (point-min))
         (let ((rx (concat "^\\(" (regexp-quote (if (symbolp sym)
                                                    (symbol-name sym)
                                                  sym))
                           "[^ \t]*\\)"))
               (res '()))
           (while (not (eobp))
             (if (not (re-search-forward rx nil t))
                 (goto-char (point-max))
               (let ((str (match-string-no-properties 1)))
                 (if (not (equal str (car res)))
                     (push str res)))
               (forward-char 1)))
           res))))

(defun scm-complete-file-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir "."))))
    (if dir
        (mapcar #'(lambda (f) (concat dir f)) res)
      res)))

(defun scm-complete-directory-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir ".")))
         (res2 (if dir (mapcar #'(lambda (f) (concat dir f)) res) res)))
    (remove-if-not #'file-directory-p res2)))

(defun scm-string-completer (type)
  (case type
    ((filename)
     '(scm-complete-file-name file-name-nondirectory))
    ((directory)
     '(scm-complete-directory-name file-name-nondirectory))
    (t
     (cond
      ((and (consp type) (eq 'string (car type)))
       (cadr type))
      ((and (consp type) (eq 'or (car type)))
       (car (delete nil (mapcar #'scm-string-completer (cdr type)))))))))

(defun scm-apply-string-completer (cmpl sym)
  (let ((func (if (consp cmpl) (car cmpl) cmpl))
        (trans (and (consp cmpl) (cadr cmpl))))
    (funcall func trans sym)))

(defun scm-smart-complete (&optional arg)
  (interactive "P")
  (let* ((end (point))
         (start (save-excursion (skip-syntax-backward "w_") (point)))
         (sym (buffer-substring-no-properties start end))
         (in-str-p (scm-in-string-p))
         (x (save-excursion
              (if in-str-p (scm-beginning-of-string))
              (scm-enclosing-2-sexp-prefixes)))
         (inner-proc (car x))
         (inner-pos (cadr x))
         (outer-proc (caddr x))
         (outer-pos (cadddr x))
         (env (save-excursion
                (if in-str-p (scm-beginning-of-string))
                (scm-current-env)))
         (outer-spec (scm-env-lookup env outer-proc))
         (outer-type (scm-translate-type (cadr outer-spec)))
         (inner-spec (scm-env-lookup env inner-proc))
         (inner-type (scm-translate-type (cadr inner-spec))))
    (cond
     ;; return all env symbols when a prefix arg is given
     (arg
      (scm-do-completion sym (scm-env-filter #'(lambda (x) t) env)))
     ;; allow different types of strings
     (in-str-p
      (let* ((param-type
              (and (consp inner-type)
                   (eq 'lambda (car inner-type))
                   (scm-lookup-type (cadr inner-type) inner-pos)))
             (completer (or (scm-string-completer param-type)
                            '(scm-complete-file-name
                              file-name-nondirectory))))
        (scm-do-completion
         ;;(if (consp completer) (funcall (cadr completer) sym) sym)
         sym
         (scm-apply-string-completer completer sym))))
     ;; outer special
     ((and (consp outer-type)
           (eq 'special (car outer-type))
           (cadddr outer-type))
      (scm-do-completion sym (funcall (cadddr outer-type) sym)))
     ;; inner special
     ((and (consp inner-type)
           (eq 'special (car inner-type))
           (caddr inner-type))
      (scm-do-completion sym (funcall (caddr inner-type) sym)))
     ;; completing inner procedure, complete procedures with a
     ;; matching return type
     ((and (consp outer-type)
           (eq 'lambda (car outer-type))
           (not (zerop outer-pos))
           (scm-nth* (- outer-pos 1) (cadr outer-type))
           (or (zerop inner-pos)
               (and (>= 1 inner-pos)
                    (consp inner-type)
                    (eq 'lambda (car inner-type))
                    (let ((param-type
                           (scm-lookup-type (cadr inner-type) inner-pos)))
                      (and (consp param-type)
                           (eq 'lambda (car param-type))
                           (eq (caddr inner-type) (caddr param-type)))))))
      (let ((want-type (scm-lookup-type (cadr outer-type) outer-pos)))
        (scm-do-completion
         sym
         (scm-env-filter
          #'(lambda (x)
              (let ((type (cadr x)))
                (or (memq type '(procedure object nil))
                    (and (consp type)
                         (or (and (eq 'syntax (car type))
                                  (not (eq 'undefined (caddr type))))
                             (and (eq 'lambda (car type))
                                  (scm-type-match-p (caddr type)
                                                       want-type)))))))
          env))))
     ;; completing a normal parameter
     ((and inner-proc
           (not (zerop inner-pos))
           (consp inner-type)
           (eq 'lambda (car inner-type)))
      (let* ((param-type (scm-lookup-type (cadr inner-type) inner-pos))
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
                              (scm-lookup-type (cadr outer-type)
                                                  outer-pos)))
                         (and (consp outer-param-type)
                              (eq 'flags (car outer-param-type))
                              (memq (scm-translate-type param-type)
                                    '(number complex real rational integer))
                              (memq (scm-translate-type (caddr inner-type))
                                    '(number complex real rational integer))
                              (cdr outer-param-type))))))
             (base-type (if set-or-flags
                            (if (and (consp param-type)
                                     (eq 'set (car param-type)))
                                (scm-translate-type (cadr param-type))
                              'integer)
                            param-type))
             (base-completions
              (scm-env-filter
               #'(lambda (x)
                   (and (not (and (consp (cadr x)) (eq 'syntax (caadr x))))
                        (scm-type-match-p (cadr x) base-type)))
               env))
             (str-completions
              (let ((completer (scm-string-completer base-type)))
                (and
                 completer
                 (scm-apply-string-completer completer sym)))))
        (scm-do-completion
         sym
         (append set-or-flags base-completions)
         str-completions)))
     ;; completing a function
     ((zerop inner-pos)
      (scm-do-completion
       sym
       (scm-env-filter
        #'(lambda (x)
            (or (null (cdr x))
                (memq (cadr x) '(procedure object nil))
                (and (consp (cadr x))
                     (memq (caadr x) '(lambda syntax special)))))
        env)))
     ;; complete everything
     (t
      (scm-do-completion sym (scm-env-filter #'(lambda (x) t) env))))))

(defun scm-complete-or-indent (&optional arg)
  (interactive "P")
  (let* ((end (point))
         (func
          (save-excursion
            (beginning-of-line)
            (if (re-search-forward "\\S-" end t)
                'scm-smart-complete
              'lisp-indent-line))))
    (funcall func arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional indentation handling

(defvar calculate-lisp-indent-last-sexp)

;; Copied from scm-indent-function, but ignore
;; scm-indent-function property for local variables.
(defun scm-smart-indent-function (indent-point state)
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
             (method (and (not (assq function-sym (scm-current-local-vars)))
                          (get function-sym 'scm-indent-function))))
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

(defun scm-object-to-string (obj)
  (with-output-to-string
    (princ (scm-elisp-to-scheme-string obj))))

;;TODO consider max recursive depth?
(defun scm-elisp-to-scheme-string (obj)
  (cond
   ((eq obj nil)
    "()")
   ((atom obj)
    obj)
   ((eq (car obj) 'quote)
    (let (ret)
      (mapcar
       (lambda (x)
	 (setq ret (cons (scm-elisp-to-scheme-string x) ret)))
       (cdr obj))
      (mapconcat 'scm-object-to-string ret " ")))
   (t
    (mapcar
     (lambda (x)
       (scm-elisp-to-scheme-string x))
     obj))))

(provide 'scm-env)

;;; scm-env.el ends here
