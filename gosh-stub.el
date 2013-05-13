
;;;TODO:
;; * 

(eval-when-compile
  (require 'cl))

(require 'gosh-mode)

;; TODO
;; * To construct keyword search following keyword
;; define-binary define-post-unary define-unary define-nary
;; define-cise-stmt define-cise-macro define-cise-expr
;; define-referencer


(defconst gosh-stub-keyword-list
  '(
    "define-cfn"
    "define-type"
    "define-symbol"
    "define-variable"
    "define-constant"
    "define-enum"
    "define-enum-conditionally"
    "define-keyword"
    "define-cproc"
    "define-cgeneric"
    "define-cmethod"
    "define-cclass"
    "define-cptr"

    "include" "initcode" "declcode" "begin" "eval*"
    "code" "call" "expr" "body"
    "result" "return" 

    "when" "unless" "else" "cond" "case" "case/fallthrough"
    "for" "loop" "for-each" "dolist" "pair-for-each" "dopairs" "dotimes"

    "break" "continue" "label" "goto"
    
    ".if" ".cond" ".undef" ".include"

    "cast"
    
    "*" "&" "->" "ref"
    ))

(defconst gosh-stub-keyword-regexp
  (concat "(" (regexp-opt gosh-stub-keyword-list t)))

(defun gosh-stub-font-lock-keywords (bound)
  (re-search-forward gosh-stub-keyword-regexp bound t))

;; font lock user customizable
(font-lock-add-keywords
 'gosh-stub-mode
 `(("\\`#.+" 0 font-lock-comment-delimiter-face)
   ("(\\(define\\(?:[^\s\t]*\\)\\)\\_>[\s\t]+(?\\([^\s\t\n]+\\)"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face))
   (gosh-stub-font-lock-keywords 1 font-lock-keyword-face)
   ))

(defvar gosh-stub-genstub-script
  ;;TODO only default?
  (locate-file "genstub" gosh-default-load-path))

(defun gosh-stub--process-buffer ()
  (let ((buf (get-buffer-create "*Gosh Stub*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    buf))

(defun gosh-stub-scheme-name->c-name (deftype libname name)
  (setq name (replace-regexp-in-string "-" "_" name))
  (cond
   ((string= deftype "define-cproc")
    (format "%s_%s" libname name))
   ((string= deftype "define-cfn")
    name)
   (t name)))

(defun gosh-stub-stub-file->libname (stubfile)
  (let ((basename (file-name-nondirectory stubfile)))
    (and (string-match "^\\(.+\\)\\.stub$" basename)
         (match-string 1 basename))))

(defun gosh-stub-current-defname ()
  (save-excursion
    (cond
     ((re-search-backward "^[ \t]*(\\(define-[^ \t]+\\)[ \t]+\\([^ \t]+\\)" nil t)
      (list (match-string-no-properties 1)
            (match-string-no-properties 2)))
     (t nil))))

(defun gosh-stub--popup-cdefine (sfile &optional def)
  (let* ((libname (gosh-stub-stub-file->libname sfile))
         (cfile (expand-file-name (format "%s.c" libname) default-directory))
         (cbuffer (get-file-buffer cfile))
         (existsp cbuffer)
         (cbuffer (or cbuffer (find-file-noselect cfile)))
         (swin (selected-window))
         pos)
    (with-current-buffer cbuffer
      (when existsp
        (revert-buffer nil t))
      (setq pos
            (catch 'done
              (cond
               ((null def)
                (point-min))
               (t
                (destructuring-bind (deftype sname) def
                  (let ((cname (gosh-stub-scheme-name->c-name deftype libname sname)))
                    (goto-char (point-min))
                    (let ((regexp (format "^static .*\\_<%s\\_>" (regexp-quote cname))))
                      (when (re-search-forward regexp nil t)
                        (throw 'done (line-beginning-position))))
                    (when (re-search-forward (format "\\_<%s\\_>" (regexp-quote cname)) nil t)
                      (throw 'done (line-beginning-position)))))
                ;; default
                (point-min))))))
    (pop-to-buffer cbuffer)
    (let ((cwin (selected-window)))
      (set-window-start cwin pos))
    (goto-char pos)
    (select-window swin)))

(defvar gosh-stub-mode-map nil)

(unless gosh-stub-mode-map

  (let ((map (or gosh-stub-mode-map (make-sparse-keymap))))

    (define-key map "\C-c?" 'gosh-show-info)
    ;;TODO change keybind
    (define-key map "\C-c\C-c" 'gosh-stub-genstub)

    (define-key map ")" 'gosh-closing-insert-paren)
    (define-key map "]" 'gosh-closing-insert-bracket)
    (define-key map "(" 'gosh-opening-insert-paren)
    (define-key map "[" 'gosh-opening-insert-bracket)

    (setq gosh-stub-mode-map map)))

(defvar gosh-stub-mode-syntax-table nil)
(unless gosh-stub-mode-syntax-table

  (let ((table (copy-syntax-table scheme-mode-syntax-table)))

    (modify-syntax-entry ?: "." table)

    (setq gosh-stub-mode-syntax-table table)))

(defvar gosh-stub-mode-abbrev-table nil)
(define-abbrev-table 'gosh-stub-mode-abbrev-table ())

;;;###autoload
(define-derived-mode gosh-stub-mode scheme-mode
  "Gosh Stub"
  (use-local-map gosh-stub-mode-map)
  (run-mode-hooks 'gosh-stub-mode-hook))

(defun gosh-stub-genstub ()
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p "Save buffer? "))
    (save-buffer))
  (let ((file buffer-file-name)
        (def (gosh-stub-current-defname)))
    (gosh-stub-genstub-maybe file)
    (gosh-stub--popup-cdefine file def)))

(defun gosh-stub-genstub-maybe (sfile)
  (let* ((libname (gosh-stub-stub-file->libname sfile))
         (default-directory (file-name-directory sfile))
         (name (file-name-nondirectory buffer-file-name))
         (cfile (format "%s.c" libname)))
    (when (file-newer-than-file-p sfile cfile)
      (message "Compiling %s..." name)
      (let ((buf (gosh-stub--process-buffer)))
        (unless (= (call-process gosh-default-command-internal
                                 nil buf nil
                                 gosh-stub-genstub-script
                                 name) 0)
          (save-selected-window
            (pop-to-buffer buf)
            (error "genstub failed"))))
      (message "%s done."(current-message)))))

;;TODO prepare for gauche-dev
;; (gosh-info-lookup-add-help 'gosh-stub-mode)

(provide 'gosh-stub)
