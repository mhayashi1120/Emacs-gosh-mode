
;;;TODO:
;; * 


;;;###autoload
(define-derived-mode gosh-stub-mode scheme-mode
  "Gosh Stub"
  (use-local-map gosh-stub-mode-map)
  (run-mode-hooks 'gosh-stub-mode-hook))

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

(defun gosh-stub-genstub ()
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p "Save buffer? "))
    (save-buffer))
  (let ((default-directory (file-name-directory buffer-file-name))
        (name (file-name-nondirectory buffer-file-name)))
    (message "Compiling %s..." name)
    (let ((buf (gosh-stub--process-buffer)))
      (unless (= (call-process gosh-default-command-internal
                               nil buf nil
                               gosh-stub-genstub-script
                               name) 0)
        (save-selected-window
          (pop-to-buffer buf)
          (error "genstub failed"))))
    (message "%s done."(current-message))))

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

;;TODO goto gosh-mode.el
(gosh-info-lookup-add-help 'gosh-stub-mode)

(provide 'gosh-stub)
