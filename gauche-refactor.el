;;; gauche-refactor.el --- Programing language Gauche refactoring tools.


;;; Commentary:
;; 

(require 'refactor)
(require 'cmuscheme)

;;; Code:

(defvar gauche-refactor-read-symbol-history nil)

(defun gauche-refactor-trim-expression (s)
  (if (string-match "^[ \t\n]*\\(\\(?:.\\|\n\\)*\\)" s)
      (match-string 1 s)
    s))

(defun gauche-refactor-read-sexp-string ()
  (gauche-refactor-trim-expression
   (buffer-substring (point)
		     (progn (forward-sexp 1) (point)))))

(defun gauche-refactor-goto-top-of-form ()
  (let ((continue t))
    (condition-case nil
	(progn
	  (while (not (bobp))
	    (backward-sexp))
	  (when (bobp)
	    (setq continue nil)
	    (when (re-search-forward "^[ \t]*(")
	      (goto-char (match-beginning 0)))))
      (scan-error 
       (backward-char 1)))
    continue))

;;TODO define-method define-constant define-syntax
(defun gauche-refactor-find-binding-region (name)
  (catch 'done
    (let ((name-regexp (refactor-create-regexp name)))
      (save-excursion
	(while (gauche-refactor-goto-top-of-form)
	  (let ((start (point))
		(end (save-excursion (forward-sexp) (point)))
		function arg)
	    ;; skip parenthese
	    (forward-char)
	    (setq function (gauche-refactor-read-sexp-string))
	    (cond
	     ((member function '("let" "let*" "letrec" "let1"
				 "if-let1" "rlet" "and-let*" "fluid-let"
				 "receive" "lambda"))
	      ;;TODO named let
	      ;; FIXME incorrect about let* and let
	      (setq arg (gauche-refactor-read-sexp-string))
	      (when (string-match name-regexp arg)
		(throw 'done (cons start end))))
	     ((member function '("define"))
	      (setq arg (gauche-refactor-read-sexp-string))
	      (when (string-match name-regexp arg)
		;;FIXME dirty
		(goto-char start)
		(gauche-refactor-goto-top-of-form)
		(let ((start (point))
		      (end (save-excursion (forward-sexp) (point))))
		  (unless (save-excursion 
			    (gauche-refactor-goto-top-of-form))
		    ;; Top of file
		    (setq end (point-max)))
		  (throw 'done (cons start end))))))
	    (goto-char start)))))
    nil))

(defun gauche-refactor-rename-symbol-read-args ()
  (refactor-rename-symbol-read-args 'gauche-refactor-read-symbol-history))

(defun gauche-refactor-find-executable-scripts ()
  (let (list)
    (mapc
     (lambda (path)
       (mapc
	(lambda (file)
	  (when (and (file-writable-p file)
		     (not (eq (car (file-attributes file)) t)))
	    (with-temp-buffer
	      (let ((coding-system-for-read 'raw-text))
		(insert-file-contents file nil 0 256))
	      (goto-char (point-min))
	      (when (looking-at "#!.*/gosh\\(\\.exe\\)?$")
		(setq list (cons file list))))))
	;;TODO dired?
	(directory-files path t dired-re-no-dot)))
     exec-path)
    list))

(defun gauche-refactor-rename-symbol (old-name new-name)
  "Rename symbol at point."
  (interactive (gauche-refactor-rename-symbol-read-args))
  (let (region)
    (setq region (gauche-refactor-find-binding-region old-name))
    (refactor-rename-region old-name new-name region)))

;;TODO
;; *load-path* files
;; writable PATH files?
;; name -> dwim?
;; this symbol is not defined in this module.
;;  jump to the module and try again.
(defun gauche-refactor-rename-symbol-afaiui (old-name new-name)
  "Rename symbol As Far As I Understand It"
  ;; (split-string (getenv "GAUCHE_LOAD_PATH") path-separator)
  ;; (scheme-current-globals)
  )

(provide 'gauche-refactor)

;;; gauche-refactor.el ends here
