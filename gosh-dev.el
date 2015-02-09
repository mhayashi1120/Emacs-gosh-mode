;;; gosh-dev.el --- Development tools for gosh-mode.el


;;; Commentary:
;; 

;;; Code:


(defcustom gosh-gauche-source-directory nil
  "TODO move to gosh-mode.el"
  :group 'gosh-mode
  :type 'directory)

(defun gosh-dev-check-reader ()
  (interactive)
  (mapcar
   (lambda (x) (gosh-dev--check-reader-for-file x))
   (gosh-dev-find-scheme-files gosh-gauche-source-directory)))

(defun gosh-dev-find-scheme-files (dir)
  (split-string
   (shell-command-to-string
    (format "find %s \\( -name .\\?\\* -prune -or -true \\) -type f -name \\*.scm" dir)) "\n" t))

(defun gosh-dev--check-reader-for-file (file)
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((start (point)))
            (condition-case err
                (let ((sexp (gosh-read)))
                  (gosh-reader-ignore))
              (error
               (let ((text (buffer-substring start (point))))
                 (signal (car err) (list (format "%s %s" text (cdr err))))))))))
    (error 
     (insert (format "File: %s Error: %s\n" file err)))))

(provide 'gosh-dev)

;;; gosh-dev.el ends here
