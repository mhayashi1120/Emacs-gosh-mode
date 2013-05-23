;;; gosh-dev.el --- Development tools for gosh-mode.el


;;; Commentary:
;; 

;;; Code:



(defun gosh-dev-find-scheme-files (dir)
  (split-string
   (shell-command-to-string
    (format "find %s \\( -name .\\?\\* -prune -or -true \\) -type f -name \\*.scm" dir)) "\n" t))

(defun gosh-dev--check-reader (file)
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (gosh-read)
          (gosh-reader--ignore)))
    (error 
     (insert (format "File: %s Error: %s\n" file err)))))



;;TODO remove duplicated values *gosh-scheme-srfi-info*
(defun gosh-dev-parse-texi (texi)
  (with-temp-buffer
    (insert-file-contents texi)
    (let (ret)
      (goto-char (point-min))
      (while (re-search-forward "^@\\(defunx?\\|defmacx?\\|defspecx?\\) \\(.*\\)" nil t)
        (let* ((type (match-string 1))
               (def (gosh-dev-split-define (match-string 2)))
               (name (intern (car def)))
               (args (gosh-dev-intern-string-args (cdr def))))
          (cond
           ((eq name '{))
           (t
            (setq ret (cons (cons name 
                                  (list (list (if (string-match "defmacx?\\|defspecx?" type) 'syntax 'lambda)
                                              (gosh-dev-create-parsing-args args))))
                            ret))))))
      (goto-char (point-min))
      (while (re-search-forward "^@\\(?:deffnx?\\) {\\([^}]+\\)} \\(.*\\)" nil t)
        (let* ((indicate (match-string 1))
               (def (gosh-dev-split-define (match-string 2)))
               (name (intern (car def)))
               (args (gosh-dev-intern-string-args (cdr def))))
          (cond
           ((eq name '{))
           ((string= indicate "EC Qualifier"))
           (t
            (setq ret (cons (cons name 
                                  (list (list 'lambda (gosh-dev-create-parsing-args args))))
                            ret))))))
      (nreverse ret))))

(defun gosh-dev-split-define (string)
  (let (arg args)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (condition-case err
          (while t
            (setq a (read (current-buffer)))
            (setq args (cons (gosh-dev-sexp-to-string a) args)))
        (error nil))
      (nreverse args))))

(defun gosh-dev-sexp-to-string (sexp)
  (let ((tmp (prin1-to-string sexp)))
    (gosh-dev-replace-string tmp "\\" "")))

(defun gosh-dev-create-parsing-args (args)
  (let (new-args)
    (while args
      (cond
       ((eq (car args) :key)
        (setq new-args (cons (car args) new-args))
        (setq args (cdr args))
        (while args
          (setq new-args (cons (list (car args)) new-args))
          (setq args (cdr args))))
       (t
        (setq new-args (cons (car args) new-args))))
      (setq args (cdr args)))
    (nreverse new-args)))

(defun gosh-dev-intern-string-args (args)
  (mapcar 
   (lambda (arg)
     (setq arg (gosh-dev-replace-string arg "@dots{}" "..."))
     (setq arg (gosh-dev-replace-string arg "@var{optional}" ":optional"))
     (setq arg (gosh-dev-replace-string arg "@code{=>}" "'=>'"))
     (setq arg (gosh-dev-replace-all-macro arg))
     (intern arg))
   args))

(defun gosh-dev-replace-all-macro (string)
  (let ((str string))
    (setq str (gosh-dev-replace-macro str "var"))
    (setq str (gosh-dev-replace-macro str "code"))
    str))

(defun gosh-dev-replace-macro (string macro)
  (let ((regexp (format "@%s{\\([^}]*\\)}" macro))
        (str string))
    (while (string-match regexp str)
      (setq str
            (concat (substring str 0 (match-beginning 0)) 
                    (match-string 1 str)
                    (substring str (match-end 0)))))
    str))

(defun gosh-dev-replace-string (string from to)
  (let ((str string)
        (case-fold-search))
    (while (string-match (regexp-quote from) str)
      (setq str 
            (concat (substring str 0 (match-beginning 0)) 
                    to
                    (substring str (match-end 0)))))
    str))

(defun gosh-dev-parse-and-insert (file)
  (let ((exports (gosh-dev-parse-texi file)))
    (insert "(defconst *gosh-documented-exports*\n")
    (insert "'(\n")
     (mapc
      (lambda (exp)
        (insert (prin1-to-string exp) "\n"))
      exports)
     (insert "))\n")))


;; (gosh-dev-parse-and-insert "~/src/System/gauche/Gauche-0.9.1/doc/gauche-refe.texi")

(provide 'gosh-dev)

;;; gosh-dev.el ends here
