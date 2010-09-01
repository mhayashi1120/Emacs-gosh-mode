;;; scm-edit.el --- Programming language scheme editing utilities.


;;; Commentary:
;; 

;;; Code:

(require 'scm-env)

(defadvice scheme-send-last-sexp
  (around scm-edit-send-last-sexp-like-elisp () activate)
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

(defun scm-auto-complete-initialize ()
  (add-to-list 'ac-sources 'ac-source-scm-edit-functions))

(defun scm-auto-complete-candidates ()
  (mapcar
   (lambda (c)
     (cond
      ((symbolp (car c))
       (symbol-name (car c)))))
   (scm-current-imports)))

(when (require 'auto-complete-config nil t)

  (ac-config-default)

  (ac-define-source scm-edit-functions
    '((candidates . scm-auto-complete-candidates)
      (symbol . "f")
      (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
      (cache)))

  (add-hook 'scheme-mode-hook 'scm-auto-complete-initialize))

(provide 'scm-edit)

;;; scm-edit.el ends here
