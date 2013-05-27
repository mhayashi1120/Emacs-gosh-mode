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
          (gosh-reader-ignore)))
    (error 
     (insert (format "File: %s Error: %s\n" file err)))))

(provide 'gosh-dev)

;;; gosh-dev.el ends here
