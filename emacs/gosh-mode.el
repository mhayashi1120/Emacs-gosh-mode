;;; gosh-mode.el --- Programming language gauche editing tools.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: lisp gauche scheme edit
;; URL: https://github.com/mhayashi1120/Emacs-gosh-mode/raw/master/gosh-mode.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.1.4

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; 

;; gosh-mode is forked from scheme-complete.el
;; Many code is duplicated but specialize to Gauche.

;;; TODO:

;; * dynamic indent?
;;   define indent rule by regexp?
;;   to indent macro form
;;   macro indent rule differ each buffer...
;; * cmuscheme C-c C-t to trace. what is this?
;; * split backend to?
;;   each executable script.
;;   module script. (all module have one backend)
;; * create parser process that is separated from backend process?

;; scheme-send-last-sexp to any keybind

;; gosh-eval-buffer to C-c C-b
;; gosh-eval-region to C-c TODO

;; * unload `user' module except followings
;;   *program-name*, *argv*

;; * regulate gosh-sticky-* gosh-eval-*

;; * Load automatically if module file is on the *load-path*?
;;   Bad idea. 

;; * gosh-show-info
;;   when :prefix symbol

;;; Code:


(defgroup gosh-mode nil
  "Gauche script editing mode."
  :group 'lisp
  :prefix "gosh-")

(defvar gosh-mode-version "0.1.4")



(eval-when-compile
  (require 'cl)
  (require 'gosh-const))

(require 'gosh-const)
(require 'eldoc nil t)
(require 'scheme)
(require 'cmuscheme)
(require 'erefactor)
(require 'info-look)
(require 'flymake)

(defvar system-type)
(defvar path-separator)
(defvar process-environment)
(defvar idle-update-delay)
(defvar current-prefix-arg)
(defvar inhibit-quit)
(defvar quit-flag)
(defvar unread-command-events)
(defvar emacs-version)
(defvar exec-path)
(defvar auto-mode-interpreter-regexp)
(defvar read-expression-map)

;;TODO move
(defvar gosh-debug nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings

(defcustom gosh-default-command "gosh"
  "Gauche program name."
  :group 'gosh-mode
  :type 'string
  :set 'gosh-set-default-command
  :initialize (lambda (s v) (set s v)))

(defvar gosh-default-command-internal nil)

(defvar gosh-default-version nil)
(defvar gosh-default-repo-path nil)
(defvar gosh-default-load-path nil)
(defvar gosh-default-command-type nil)
(defvar gosh-default-path-separator nil)
(defvar gosh-default-path-g2e nil)
(defvar gosh-default-path-e2g nil)
(defvar gosh-command-alist nil
  "List about command information following order.
COMMAND VERSION SYSLIBDIR LOAD-PATH TYPE PATH-SEPRATOR CONVERTER1 CONVERTER1"
  )

;; base/super module definitions + import module exported definitions
;; string-copy: for resolving shared structure.
(defconst gosh-module-imports-command-string
  (concat 
   "(apply append "
   "(hash-table-map (module-table (current-module)) "
   " (lambda (sym gloc) (string-copy (symbol->string sym)))) "
   "(map (lambda (mod) (map (lambda (sym) (string-copy (symbol->string sym))) (module-exports mod))) "
   " (module-imports (current-module))))\n"))

(defvar gosh-autoload-modules
  '(null gauche.hashutil user gauche scheme))

;;TODO only autoloads
;;TODO check gauche initialize process.
;; trunk/src/module.c
;; DEFINE_STATIC_MODULE

(defconst gosh-autoload-symbols-command-format
  (concat
   "(apply append (map (lambda (mod) "
   "(hash-table-map (module-table mod) "
   "(lambda (sym gloc) (string-copy (symbol->string sym))))) "
   "(fold (lambda (s r) (if-let1 m (find-module s) (cons m r) r)) '() '%s)))\n"
   ))

(defun gosh-autoload-symbols-command-string ()
  (format gosh-autoload-symbols-command-format gosh-autoload-modules))

(defconst gosh-unload-module-command-format
  (concat
   "(if-let1 m (find-module '%s)(hash-table-clear! (module-table m)))"
   ))

;; info 6.20.3 section
(defconst gosh-eval-expression-command-format
  (concat
   "(let1 port (open-output-file \"%s\") "
   "(unwind-protect "
   " (begin (standard-output-port port) (standard-error-port port) "
   " (begin0 (with-output-to-port port (lambda () %s)))) "
   " (close-output-port port)))"
   ))

(defconst gosh-backend-eval-guard-format
  (concat
   "(guard (e "
   " (else (print \"%s\" "
   " (or "
   " (and (slot-exists? e 'message) (slot-ref e 'message))"
   " \"ERROR\""
   " )))) %s)\n"))

(defconst gosh-backend-check-parenthes-format
  (concat
   "(with-input-from-file \"%s\""
   " (lambda () "
   " (let loop ((exp (read))) "
   " (unless (eof-object? exp) "
   " (loop (read))))))\n"
   ))

;; bound only `let' form
(defvar gosh-delegate-command)
(defun gosh-delegate-command-get (index)
  ;; When debugging gosh-mode, execute function (ex: `eval-expression') make unbound variable error.
  (let ((command (if (and gosh-debug 
                          (not (boundp 'gosh-delegate-command)))
                     (gosh-current-executable)
                   gosh-delegate-command)))
    (nth index (assoc command gosh-command-alist))))

(defun gosh-delegate-version ()
  (gosh-delegate-command-get 1))
(defun gosh-delegate-repo-path ()
  (gosh-delegate-command-get 2))
(defun gosh-delegate-load-path ()
  (gosh-delegate-command-get 3))
(defun gosh-delegate-command-type ()
  (gosh-delegate-command-get 4))
(defun gosh-delegate-path-separator ()
  (gosh-delegate-command-get 5))
(defun gosh-delegate-path-g2e ()
  (gosh-delegate-command-get 6))
(defun gosh-delegate-path-e2g ()
  (gosh-delegate-command-get 7))

(defun gosh-register-command (command)
  (let* ((full (gosh--check-command command)))
    (unless full
      (error "Unable recognize as gosh command"))
    (or (assoc full gosh-command-alist)
        (let* ((output (gosh--call-command->string full full "-V"))
               (ver (when (string-match "version[ \t]+\\([0-9][0-9.]+\\)" output)
                      (match-string 1 output)))
               (type (cond 
                      ((string-match "mingw32$" output) 'mingw32)
                      ((string-match "cygwin$" output) 'cygwin)
                      (t 'unix)))
               (sep (case type (mingw32 ";") (t ":")))
               (repo (let* ((res (gosh--call-command->string full "gauche-config" "--syslibdir"))
                            (res (substring res 0 -1))) ;remove trailing newline
                       (let* ((dir (file-name-directory res))
                              (dir2 (file-name-directory
                                     (directory-file-name dir))))
                         (directory-file-name dir2))))
               (g2e (case type (cygwin 'gosh-cygpath->emacs-path) (t 'identity)))
               (e2g (case type (cygwin 'gosh-emacs-path->cygpath) (t 'identity)))
               (path (mapcar g2e (gosh-exact-load-path full t)))
               (item (list full ver repo path type sep g2e e2g)))
          (setq gosh-command-alist
                (cons item gosh-command-alist))
          item))))

(defun gosh-switch-default-command (command)
  "Switch gosh command (ex: trunk <-> release)"
  (interactive
   (let ((command (completing-read 
                   "Command: " gosh-command-alist nil nil
                   gosh-default-command-internal)))
     (list command)))
  (gosh-default-initialize command))

(defun gosh-set-default-command (dummy value)
  (gosh-default-initialize value))

(defun gosh-default-initialize (&optional default-command)
  (when default-command
    (setq gosh-default-command default-command))
  (let ((info (gosh-register-command gosh-default-command)))
    (setq gosh-default-command-internal (nth 0 info)
          gosh-default-version (nth 1 info)
          gosh-default-repo-path (nth 2 info)
          gosh-default-load-path (nth 3 info)
          gosh-default-command-type (nth 4 info)
          gosh-default-path-separator (nth 5 info)
          gosh-default-path-g2e (nth 6 info)
          gosh-default-path-e2g (nth 7 info))))

;; Execute when loading.
(defun gosh-initialize ()
  (gosh-default-initialize)
  (gosh-ac-initialize)
  (gosh-info-lookup-initialize))

(defun gosh--call-command->string (gosh command &rest args)
  (let ((dir (file-name-directory gosh)))
    (with-temp-buffer
      (apply 'call-process (expand-file-name command dir) nil (current-buffer) nil args)
      (buffer-string))))

(defun gosh--check-command (command)
  (let ((full (executable-find command)))
    (when (and full (string-match "/gosh\\(\\.exe\\)?$" full))
      full)))

;;TODO delete duplicates (preserve seq)
(defun gosh-available-modules ()
  "All module symbols in *load-path*"
  (let ((paths (gosh-load-path)))
    (mapcar
     (lambda (f) 
       (subst-char-in-string ?/ ?. f))
     (mapcar
      'file-name-sans-extension
      (gosh-append-map
       (lambda (dir)
         (let ((len (length dir)))
           (mapcar 
            (lambda (f) (substring f (+ 1 len)))
            (gosh-directory-tree-files dir "\\.scm$"))))
       (mapcar 'directory-file-name paths))))))

(defun gosh--module->file (mod)
  ;;TODO current-executable and switch load-path
  (let* ((file (concat (subst-char-in-string ?. ?/ (symbol-name mod)) ".scm"))
         (dir
          (gosh-any-file-in-path
           file
           (gosh-load-path))))
    (when dir
      (expand-file-name file dir))))

(defun gosh-environ-load-path ()
  (let ((path (split-string (or (getenv "GAUCHE_LOAD_PATH") "")
                            gosh-default-path-separator)))
    (mapcar gosh-default-path-g2e path)))

(defun gosh-load-path ()
  (append 
   (gosh-environ-load-path) 
   gosh-default-load-path))

(defun gosh-exact-load-path (command &optional system-only)
  (let ((process-environment (copy-sequence process-environment))
        (list '())
        (args '()))
    (when system-only
      (setenv "GAUCHE_LOAD_PATH" nil))
    (setq args (list "-b" "-e" "(map print *load-path*)"))
    (with-temp-buffer
      (apply 'call-process command nil (current-buffer) nil args)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((file (buffer-substring (line-beginning-position) (line-end-position))))
          (setq list (cons file list)))
        (forward-line 1))
      (nreverse list))))

(defun gosh-jump-thingatpt ()
  "Jumpt to current symbol definition.

TODO prefixed symbol
  push-mark"
  (interactive)
  (let* ((sym (gosh-parse-symbol-at-point))
         (modules 
          (append
           (ignore-errors (gosh-parse-current-base-modules))
           (ignore-errors (gosh-parse-buffer-import-modules)))))
    (catch 'found
      (when (assq sym (gosh-parse-current-globals t))
        (gosh-jump-to-definition sym)
        (throw 'found t))
      (mapc
       (lambda (f)
         (when (memq sym (gosh-exports-functions f))
           (let ((buffer (get-file-buffer f)))
             (set-buffer (or buffer (find-file-noselect f)))
             (when (gosh-jump-to-definition sym)
               (switch-to-buffer (current-buffer))
               (throw 'found t)))))
       (gosh-guessed-modules modules (symbol-name sym)))
      (let ((file (gosh--module->file sym)))
        (when file
          (switch-to-buffer (find-file-noselect file))
          (throw 'found t)))
      (message "Not found definition %s" sym))))

(defun gosh-jump-to-definition (definition)
  (let ((name (symbol-name definition))
        (first (point)))
    (goto-char (point-min))
    (if (re-search-forward (format "^[ \t]*(def.*\\_<%s\\_>" (regexp-quote name)) nil t)
        (forward-line 0)
      (goto-char first)
      nil)))

(defun gosh-guessed-modules (imported symbol)
  (let ((sym (or (and (stringp symbol) symbol)
                 (symbol-name symbol)))
        prefix words 
        first second third)
    (unless (string-match "^\\([^-]+\\)" sym)
      (error "Unable find"))
    (setq prefix (match-string 1 sym))
    (setq words (split-string sym "\\b" t))
    (mapc
     (lambda (mname)
       (let* ((sym (intern mname))
              (names (split-string mname "[.]"))
              (file (gosh--module->file sym)))
         (cond
          ((memq sym imported)
           (push file first))
          ((member prefix names)
           (push file second))
          ((intersection words names :test 'string=)
           (push file second))
          (t
           (push file third)))))
     (gosh-available-modules))
    (remove nil (append first second third))))

(defun gosh-exports-functions (file)
  (gosh-with-find-file file
    (gosh-parse-current-exports)))

(defun gosh-show-info (symbol-name)
  "Popup `info' buffer"
  (interactive 
   (let ((sym (gosh-parse-symbol-at-point)))
     (list (symbol-name sym))))
  (let (message-log-max)
    (info-lookup-symbol symbol-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sticky to emacs

(defface gosh-modeline-normal-face
  '((((class color) (min-colors 88) (background light))
     :inherit font-lock-function-name-face)
    (((class grayscale mono) (background dark))
     :inherit font-lock-function-name-face)
    (((class color) (background light))
     :inherit font-lock-function-name-face)
    (((class color) (min-colors 88) (background dark))
     :foreground "Blue1")
    (((background dark))
     :foreground "Blue1")
    (t
     :foreground "LightSkyBlue"))
  "Face used to highlight mode line."
  :group 'gosh-mode)

;;TODO no need? or rename?
(defface gosh-modeline-lightdown-face
  '((t
     :foreground "White"))
  "TODO Face used to highlight mode line."
  :group 'gosh-mode)

(defface gosh-modeline-working-face
  '((((class color) (min-colors 88) (background light))
     :inherit font-lock-variable-name-face)
    (((class grayscale mono) (background dark))
     :inherit font-lock-variable-name-face)
    (((class color) (background light))
     :inherit font-lock-variable-name-face)
    (((class color) (min-colors 88) (background dark))
     :foreground "GreenYellow")
    (((background dark))
     :foreground "GreenYellow")
    (t
     :foreground "LightSkyBlue"))
  "Face used to highlight mode line when processs is working."
  :group 'gosh-mode)

(defface gosh-modeline-error-face
  '((((class color) (min-colors 88) (background light))
     :inherit font-lock-warning-face)
    (((class grayscale mono) (background dark))
     :inherit font-lock-warning-face)
    (((class color) (background light))
     :inherit font-lock-warning-face)
    (((class color) (min-colors 88) (background dark))
     :foreground "Red1")
    (((background dark))
     :foreground "Red1")
    (t
     :foreground "OrangeRed1"))
  "Face used to error highlight mode line module name."
  :group 'gosh-mode)

(defface gosh-momentary-message-face
  '((((background dark))
     :foreground "LightSkyBlue" :bold t)
    (t
     :foreground "Blue1" :bold t))
  "Face used to momentary message."
  :group 'gosh-mode)

(defvar gosh-sticky-mode-update-timer nil)

(defvar gosh-sticky-modeline-format
  `(
    " "
    (:eval gosh-sticky-modeline-validate)
    " "
    (:propertize gosh-sticky-which-module-current
                 face gosh-modeline-normal-face)
    "=>"
    (:eval gosh-sticky-test-result))
  "Format for displaying the function in the mode line.")
(put 'gosh-sticky-modeline-format 'risky-local-variable t)

(defvar gosh-sticky-validated-time nil)
(make-variable-buffer-local 'gosh-sticky-validated-time)

(defvar gosh-sticky-which-module-current nil)
(make-variable-buffer-local 'gosh-sticky-which-module-current)
(put 'gosh-sticky-which-module-current 'risky-local-variable t)

(defvar gosh-sticky-modeline-validate nil)
(make-variable-buffer-local 'gosh-sticky-modeline-validate)
(put 'gosh-sticky-modeline-validate 'risky-local-variable t)

(defvar gosh-sticky-test-result nil)
(make-variable-buffer-local 'gosh-sticky-test-result)
(put 'gosh-sticky-test-result 'risky-local-variable t)

(defvar gosh-mode-line-process nil)
(make-variable-buffer-local 'gosh-mode-line-process)
(put 'gosh-mode-line-process 'risky-local-variable t)

(defvar gosh-sticky-mode-update-delay 1
  "Idle delay seconds to validate current buffer.")

(defvar gosh-sticky-mode-map nil)

(let ((map (or gosh-sticky-mode-map (make-sparse-keymap))))

  (define-key map "\C-c\C-b" 'gosh-eval-buffer)
  (define-key map "\C-c\C-n" 'gosh-eval-region)
  (define-key map "\C-x\C-e" 'gosh-send-last-sexp)
  (define-key map "\M-:" 'gosh-eval-expression)
  (define-key map "\M-\C-x" 'gosh-eval-defun)

  (setq gosh-sticky-mode-map map))

(define-minor-mode gosh-sticky-mode 
  "Gosh sticky process mode.  
Evaluate s-expression, syntax check, test-module, etc."
  nil nil gosh-sticky-mode-map
  (if gosh-sticky-mode
      (add-hook 'after-save-hook 'gosh-sticky-after-save nil t)
    (remove-hook 'after-save-hook 'gosh-sticky-after-save t))
  (when (timerp gosh-sticky-mode-update-timer)
    (cancel-timer gosh-sticky-mode-update-timer))
  (setq gosh-sticky-mode-update-timer
        (run-with-idle-timer gosh-sticky-mode-update-delay t
                             'gosh-sticky-timer-process))
  (setq gosh-mode-line-process
        (and gosh-sticky-mode 
             'gosh-sticky-modeline-format))
  (gosh-sticky-reset-test)
  (gosh-sticky-backend-switch-context))

(defun gosh-sticky-mode-on ()
  (gosh-sticky-mode 1))

(defun gosh-sticky-mode-off ()
  (gosh-sticky-mode -1))

(defun gosh-sticky-timer-process ()
  ;; check buffer mode to avoid endless freeze 
  (when (and gosh-sticky-mode 
             (eq major-mode 'gosh-mode)
             (not (minibufferp (current-buffer))))
    (gosh-sticky-backend-watcher)
    (gosh-sticky-modeline-update)))

(defun gosh-sticky-backend-watcher ()
  (unless (gosh-backend-active-process)
    (gosh-sticky-backend-switch-context)))

(defun gosh-sticky-modeline-update ()
  (let* ((prev-module gosh-sticky-which-module-current)
         (module (gosh-sticky-which-module-update)))
    (when (or (gosh-sticky-buffer-was-changed-p)
              (not (equal prev-module module)))
      ;;TODO
      (gosh-sticky-reset-test)))
  (force-mode-line-update))

(defun gosh-sticky-buffer-was-changed-p ()
  "Return non-nil if current-buffer was changed after validated"
  (or (null gosh-buffer-change-time)
      (null gosh-sticky-validated-time)
      (> gosh-buffer-change-time
         gosh-sticky-validated-time)))

(defun gosh-sticky-which-module-update ()
  "Refresh `gosh-sticky-which-module-current' and return that new value."
  (setq gosh-sticky-which-module-current 
        (ignore-errors
          (gosh-parse-context-module))))

(defun gosh-sticky-validating-p ()
  (catch 'found
    (mapc
     (lambda (p)
       (when (eq (process-get p 'gosh-sticky-test-buffer) (current-buffer))
         (throw 'found t)))
     (process-list))
    nil))

(defun gosh-sticky-reset-test ()
  (setq gosh-sticky-test-result 
        '(:propertize "Unknown" face gosh-modeline-lightdown-face)))

;; Execute subprocess that have sentinel and filter
;;  subprocess load current file
;;  -> that filter make process execute test-module
;;  -> sentinel read test-module result
(defun gosh-sticky-validate-async (module)
  (let ((test-buffer (current-buffer))
        (file (gosh-sticky-backend-loading-file))
        (result-buf (generate-new-buffer " *gosh-mode-test* "))
        proc)
    (setq gosh-sticky-modeline-validate
          `(:propertize "Loading"
                        face gosh-modeline-working-face))
    (gosh-sticky-reset-test)
    (setq gosh-sticky-validated-time (float-time))
    (setq proc (start-process "Gosh test" result-buf
                              gosh-default-command-internal
                              "-i" "-u" "gauche.test" 
                              "-l" file))
    (set-process-sentinel proc 'gosh-sticky-validate-sentinel)
    (set-process-filter proc 'gosh-sticky-validate-filter)
    (process-put proc 'gosh-sticky-test-buffer test-buffer)
    (process-put proc 'gosh-sticky-test-module module)
    proc))

(defun gosh-sticky-validate-filter (proc event)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (let ((buffer (process-get proc 'gosh-sticky-test-buffer))
            (module (process-get proc 'gosh-sticky-test-module)))
        (if (not (buffer-live-p buffer))
            (gosh-sticky-close-process proc)
          (goto-char (point-max))
          (insert event)
          (goto-char (point-min))
          (cond
           ((gosh-backend-prompt-match)
            ;; remove this filter
            (set-process-filter proc nil)
            (with-current-buffer buffer
              (setq gosh-sticky-test-result 
                    `(:propertize "Checking"
                                  face gosh-modeline-working-face))
              ;; clear overlay
              (gosh-sticky-validate-remove-error)
              (setq gosh-sticky-modeline-validate
                    '(:propertize "Valid" face gosh-modeline-normal-face)))
            (process-send-string proc (format "(test-module '%s)\n" module))
            ;; to terminate gosh process
            (process-send-eof proc))
           ((re-search-forward "^gosh: \\(.*\\)" nil t)
            ;;TODO multiple error message?
            (let ((message (match-string 1)))
              (with-current-buffer buffer
                (gosh-sticky-validate-parse-error-message message)
                (setq gosh-sticky-modeline-validate
                      `(:propertize "Invalid" 
                                    face gosh-modeline-error-face
                                    help-echo ,(concat "Gosh error: " message)
                                    mouse-face mode-line-highlight)))))))))))

(defun gosh-sticky-validate-parse-error-message (message)
  (save-excursion
    (cond
     ((string-match ":line \\([0-9]+\\):\\(.*\\)" message)
      (let ((line (string-to-number (match-string 1 message)))
            (text (match-string 2 message)))
        (gosh-goto-line line)
        (when (= (point) (point-max))
          (forward-line -1))
        ;;TODO completely using flymake
        (flymake-make-overlay
         (point) (line-end-position) 
         text 'flymake-errline 'flymake-errline))))))

(defun gosh-sticky-validate-remove-error ()
  (save-restriction
    (widen)
    (mapc
     (lambda (ov)
       (when (flymake-overlay-p ov)
         (delete-overlay ov)))
     (overlays-in (point-min) (point-max)))))

(defun gosh-sticky-validate-sentinel (proc event)
  (unless (eq (process-status proc) 'run)
    (let ((buffer (process-get proc 'gosh-sticky-test-buffer)))
      (if (not (buffer-live-p buffer))
          (gosh-sticky-close-process proc)
        (with-current-buffer buffer
          (setq gosh-sticky-test-result
                (unwind-protect
                    (with-current-buffer (process-buffer proc)
                      (catch 'done
                        (unless (= (process-exit-status proc) 0)
                          (throw 'done 
                                 `(:propertize "NG" 
                                               face gosh-modeline-error-face
                                               help-echo "Gosh test error: executing error"
                                               mouse-face mode-line-highlight)))
                        (let (errors)
                          (goto-char (point-min))
                          (while (re-search-forward "ERROR:[ \t]*\\(.*\\)" nil t)
                            (setq errors (cons (match-string 1) errors)))
                          (when errors
                            (throw 'done 
                                   `(:propertize "NG" 
                                                 face gosh-modeline-error-face
                                                 help-echo ,(concat "Gosh test error: " (prin1-to-string errors))
                                                 mouse-face mode-line-highlight)))
                          `(:propertize "OK"
                                        face gosh-modeline-normal-face))))
                  (gosh-sticky-close-process proc))))))))

(defun gosh-sticky-close-process (proc)
  (delete-process proc)
  (kill-buffer (process-buffer proc)))

(defun gosh-sticky-after-save ()
  (gosh-sticky-modeline-update))

(defun gosh-run (cmd)
  "Wrapper of `run-scheme' command."
  (interactive 
   (let ((command
          (if current-prefix-arg
              (completing-read "Run Gosh: " 
                               (mapcar 
                                (lambda (c) (list (car c)))
                                gosh-command-alist)
                               nil nil gosh-default-command)
            gosh-default-command-internal)))
     (list (combine-and-quote-strings (list command "-i")))))
  (run-scheme cmd)
  (unless (eq major-mode 'gosh-inferior-mode)
    (gosh-inferior-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode definitions

(defvar gosh-mode-map nil)

(unless gosh-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scheme-mode-map)

    (define-key map "\M-\C-i" 'gosh-smart-complete)
    (define-key map "\C-c\C-j" 'gosh-jump-thingatpt)
    (define-key map "\C-c\C-u" 'gosh-test-module) ;;todo key bind is?
    (define-key map "\C-c?" 'gosh-show-info)
    (define-key map "\C-c\M-r" 'gosh-refactor-rename-symbol)
    ;; (define-key map "\C-c\er" 'gosh-refactor-rename-symbol-afaiui)
    (define-key map "\C-c\C-p" 'gosh-popup-test-result)

    (setq gosh-mode-map map)))

(defvar gosh-mode-hook nil)

(define-derived-mode gosh-mode scheme-mode "Gosh"
  "Major mode for Gauche programming."
  ;;TODO check documents
  ;; (when (boundp 'syntax-propertize-function)
  ;;   (set (make-local-variable 'syntax-propertize-function)
  ;;        'gosh-syntax-table-apply-region))
  (set (make-local-variable 'font-lock-syntactic-keywords)
       (append
        font-lock-syntactic-keywords
        gosh-font-lock-syntactic-keywords))
  (set (make-local-variable 'after-change-functions)
       'gosh-after-change-function)
  (add-to-list (make-local-variable 'mode-line-process)
               'gosh-mode-line-process
               'append)
  (setq gosh-buffer-change-time (float-time))
  (gosh-eldoc-config)
  (gosh-ac-mode-initialize)
  (use-local-map gosh-mode-map)
  (run-mode-hooks 'gosh-mode-hook))

;;
;; font-lock
;;

(defun gosh-font-lock-keywords (bound)
  ;; ignore if quack is activated
  (and (not (featurep 'quack))
       (re-search-forward gosh-defined-procedure-keyword-regexp bound t)))

(defun gosh-font-lock-basic-syntax (bound)
  ;; ignore if quack is activated
  (and (not (featurep 'quack))
       (re-search-forward gosh-basic-syntax-keyword-regexp bound t)))

;;
;; syntax
;;

(defconst gosh-font-lock-syntactic-keywords
  `(
    (,gosh-regexp-literal-regexp (1 (6)) (2 (7)) (4 (7)))
    ))

(defun gosh-syntax-table-apply-region (start end)
  (let ((modified (buffer-modified-p))
        buffer-read-only)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward gosh-regexp-literal-regexp nil t)
          (gosh-syntax-table-set-properties (match-beginning 0) (match-end 0)))))
    (set-buffer-modified-p modified)))

(defun gosh-syntax-table-put-property (beg end value)
  (put-text-property beg end 'syntax-table value (current-buffer)))

(defun gosh-syntax-table-set-properties (beg end)
  (let ((curpos beg)
        (state 0))
    (while (<= curpos end)
      (cond
       ((= state 0)
        (when (= (char-after curpos) ?#)
          ;; (6) = expression prefix, (3) = symbol
          (gosh-syntax-table-put-property curpos (1+ curpos) '(6))) 
        (setq state (+ 1 state)))

       ((= state 1)
        (when (= (char-after curpos) ?/)
          ;; (7) = string quote
          (gosh-syntax-table-put-property curpos (1+ curpos) '(7))) 
        (setq state (+ 1 state)))

       ((= state 2)
        (cond
         ;; handle backslash inside the string
         ((= (char-after curpos) ?/)
          ;; (1) = punctuation, (2) = word
          (if (= (char-before curpos) ?\\)
              (gosh-syntax-table-put-property curpos (+ curpos 2) '(2))
            (gosh-syntax-table-put-property curpos (1+ curpos) '(7))))

         ;; everything else
         (t
          nil))))
      ;; next char
      (setq curpos (+ curpos 1)))))


;;
;; inferior mode
;;

(defvar gosh-inferior-mode-map nil)

(unless gosh-inferior-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\M-\C-i" 'gosh-smart-complete)
    (define-key map "\C-c?" 'gosh-show-info)
    (define-key map "\C-i" 'gosh-inferior-smart-complete)

    (setq gosh-inferior-mode-map map)))

(defvar gosh-inferior-mode-hook nil)

(defun gosh-inferior-mode () 
  "Prepare new `major-mode' to activate `auto-complete'.

DO NOT derived `inferior-scheme-mode'. 
`run-scheme' execute `inferior-scheme-mode-hook' 
derived major mode run the hook one more time."
  (gosh-eldoc-config)
  (gosh-ac-mode-initialize)
  (when (featurep 'auto-complete)
    (add-to-list 'ac-sources 'ac-source-gosh-inferior-symbols))
  (setq major-mode 'gosh-inferior-mode)
  (setq mode-name "Inferier Gosh")
  ;; clear
  (setq gosh-inferior-autoload-functions nil)
  (set-keymap-parent gosh-inferior-mode-map inferior-scheme-mode-map)
  (use-local-map gosh-inferior-mode-map)
  (run-mode-hooks 'gosh-inferior-mode-hook))

(defun gosh-inferior-smart-complete ()
  "Smart complete in `gosh-inferior-mode'.
That candidates are gathered from current-module.
TODO but not supported with-module context."
  (interactive)
  (let ((cands (gosh-inferior-symbol-candidates))
        (sym (gosh-cmoplete-symbol-name-at-point)))
    (gosh-scheme-do-completion sym cands)))

(defun gosh-inferior-symbol-candidates ()
  (let ((proc (gosh-inferior-process)))
    (unless gosh-inferior-autoload-functions
      (setq gosh-inferior-autoload-functions
            (gosh-inferior-autoload-functions proc)))
    (append
     gosh-inferior-autoload-functions
     (gosh-inferier-import-candidates proc))))

(defun gosh-inferier-import-candidates (proc)
  "Gather symbols from PROC current-module context"
  (gosh-snatch-process-candidates proc gosh-module-imports-command-string))

(defun gosh-inferior-process ()
  (when (boundp 'scheme-buffer)
    (let ((proc (get-buffer-process scheme-buffer)))
      (and proc (eq (process-status proc) 'run) proc))))

(defvar gosh-inferior-autoload-functions nil)
(defun gosh-inferior-autoload-functions (proc)
  "Gather autoload symbols from PROC"
  (gosh-snatch-process-candidates proc (gosh-autoload-symbols-command-string)))



(defvar gosh-snatch-filter-candidates nil)
(defvar gosh-snatch-filter-string-stack nil)

(defun gosh-snatch-process-candidates (proc command)
  "Execute COMMAND in PROC"
  (let* ((filter (process-filter proc)))
    ;;TODO temporarily... 
    ;;   auto-complete timer process snatch from gosh-snatch-process-filter
    (unless (eq filter 'gosh-snatch-process-filter)
      (set-process-filter proc 'gosh-snatch-process-filter)
      (unwind-protect
          (progn
            (setq gosh-snatch-filter-candidates nil
                  gosh-snatch-filter-string-stack nil)
            (process-send-string proc command)
            (let ((inhibit-quit t))
              (while (not gosh-snatch-filter-candidates)
                (sleep-for 0.1)
                (when quit-flag
                  (setq inhibit-quit nil)))))
        (set-process-filter proc filter))
      (and (listp gosh-snatch-filter-candidates)
           gosh-snatch-filter-candidates))))

;; FIXME Unable read scheme symbol `t'.
(defun gosh-snatch-process-filter (proc event)
  (condition-case err
      (progn
        (setq gosh-snatch-filter-string-stack 
              (concat gosh-snatch-filter-string-stack event))
        (when (string-match gosh-backend-prompt-string-regexp gosh-snatch-filter-string-stack)
          ;; read scheme '() to nil but to finish up the filter change to `t'
          (setq gosh-snatch-filter-candidates
                (or
                 (read gosh-snatch-filter-string-stack)
                 t))))
    (error 
     ;; step through if error.
     (setq gosh-snatch-filter-candidates t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc

(defconst gosh-eldoc--cached-data (make-vector 3 nil))

(defvar gosh-eldoc--rotate-timer nil)

(defcustom gosh-eldoc-idle-delay nil
  "Same as `eldoc-idle-delay', but only affect `gosh-mode'.
Set this variable before open by `gosh-mode'."
  :type 'number
  :group 'gosh-mode)

(defcustom gosh-eldoc-rotate-seconds 1.5
  "Number of seconds rotating eldoc message."
  :group 'gosh-mode
  :type 'number
  :set (lambda (s v) 
         (when gosh-eldoc--rotate-timer
           (cancel-timer gosh-eldoc--rotate-timer)
           (setq gosh-eldoc--rotate-timer nil))
         (set s v)
         (gosh-eldoc-initialize-rotate-timer))
  :initialize (lambda (s v) (set s v)))

(defun gosh-eldoc-config ()
  (when (require 'eldoc nil t)
    (when (numberp gosh-eldoc-idle-delay)
      (set (make-local-variable 'eldoc-idle-delay) gosh-eldoc-idle-delay))
    (set (make-local-variable 'eldoc-documentation-function)
         'gosh-eldoc-print-current-symbol-info)
    (gosh-eldoc-initialize-rotate-timer)))

(defun gosh-eldoc-initialize-rotate-timer ()
  (unless gosh-eldoc--rotate-timer
    (setq gosh-eldoc--rotate-timer 
          ;; eldoc-rotate must be delay than eldoc
          (run-with-timer (* eldoc-idle-delay 2) 
                          gosh-eldoc-rotate-seconds
                          'gosh-eldoc-rotate-print-info))))

(defun gosh-eldoc--highlight-sexp (info-sexp highlight)
  (let ((index (nth 0 highlight))
        (sym (nth 1 highlight))
        (prev-sym (nth 2 highlight))
        (real-sexp (gosh-eldoc--normalize-fn-sexp info-sexp))
        target-exp)
    (when (or (keywordp sym)
              (keywordp prev-sym))
      (let ((key1 
             (intern-soft (substring (symbol-name 
                                      (or 
                                       (and (keywordp sym) sym)
                                       (and (keywordp prev-sym) prev-sym)))
                                     1)))
            (key2
             (or 
              (and (keywordp sym) sym)
              (and (keywordp prev-sym) prev-sym)))
            (keywords (cdr (memq :key info-sexp))))
        (setq target-exp (or (assq key1 keywords)
                             (assq key2 keywords)))))
    (unless target-exp
      (setq target-exp (nth index real-sexp)))
    (unless target-exp
      (gosh-aif (gosh-eldoc--sexp-rest-arg real-sexp)
          (setq target-exp it)))
    (concat "("
            (mapconcat
             'identity
             (mapcar
              (lambda (exp)
                (let ((str (gosh-eldoc--object->string exp)))
                  (when (eq target-exp exp)
                    (add-text-properties 0 (length str) 
                                         (list 'face 'eldoc-highlight-function-argument)
                                         str))
                  str))
              info-sexp)
             " ")
            ")")))

(defun gosh-eldoc--normalize-fn-sexp (sexp)
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

(defun gosh-eldoc--base-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((string list) (car x))
      ((set) (or (cadr x) (car x)))
      ((flags) 'integer)
      ((lambda) 'procedure)
      ((syntax) 'syntax)
      (t x))))

(defun gosh-eldoc--canonicalize-order (ls)
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

(defun gosh-eldoc--sexp->string (sexp &optional highlight)
  (if (not highlight)
      (gosh-eldoc--object->string sexp)
    (gosh-eldoc--highlight-sexp sexp highlight)))

(defun gosh-eldoc--sexp-rest-arg (sexp)
  (catch 'found
    (mapc
     (lambda (x)
       (when (and (vectorp x)
                  (> (length x) 0) 
                  (string-match "\\.\\.\\.$" (symbol-name (aref x 0))))
         (throw 'found x)))
     sexp)
    (let ((last (car (last sexp))))
      (when (and (symbolp last) (string-match "\\.\\.\\.$" (symbol-name last)))
        (throw 'found last)))
    nil))

(defun gosh-eldoc--translate-dot-cell (ls)
  (let ((res '()))
    (while (consp ls)
      (setq res (cons (car ls) res))
      (setq ls (cdr ls)))
    (when (not (null ls))
      (setq res (cons (vector (intern (concat (gosh-eldoc--object->string ls) "..."))) res)))
    (reverse res)))

(defun gosh-eldoc-rotate-print-info ()
  (condition-case err
      (when (eldoc-display-message-p)
        (when (eq (aref gosh-eldoc--cached-data 0) (point))
          (let* ((index (aref gosh-eldoc--cached-data 1))
                 (info (aref gosh-eldoc--cached-data 2))
                 (len (length info)))
            (when (> len 1)
              (aset gosh-eldoc--cached-data 1 (% (1+ index) len))
              (eldoc-message (nth index info))))))
    (error (message "gosh-eldoc error: %s" err))))

(defun gosh-eldoc-print-current-symbol-info ()
  (let* ((fnsym0 (gosh-parse-fnsym-current-sexp))
         (fnpos (if (consp fnsym0) (cadr fnsym0) 0))
         (sym (and (consp fnsym0) (gosh-nth* fnpos (car fnsym0))))
         (fnsym (cond ((atom fnsym0) fnsym0)
                      (t (caar fnsym0))))
         (env (save-excursion
                (gosh-beginning-of-string)
                (gosh-parse-current-env)))
         (spec1 (and fnsym (gosh-env-matches env fnsym t)))
         (spec2 (and sym (gosh-env-matches env sym t)))
         (string1 (and spec1 
                     (gosh-eldoc--find-and-print-strings
                      spec1 env
                      (cons fnpos (nthcdr (1- fnpos) (car fnsym0))))))
         (string2 (and (> fnpos 0)
                     spec2 
                     (gosh-eldoc--find-and-print-strings
                      spec2 env nil)))
         (strings (remove nil (append string1 string2))))
    (gosh-eldoc--cache-set (point) 0 strings)
    (car strings)))

(defun gosh-eldoc--cache-set (point index info)
  (aset gosh-eldoc--cached-data 0 point)
  (aset gosh-eldoc--cached-data 1 index)
  (aset gosh-eldoc--cached-data 2 info))

(defun gosh-eldoc--object->string (obj)
  ;; avoid `max-lisp-eval-depth' error
  (condition-case err
      (with-output-to-string
        (princ (gosh-eldoc--elisp->scheme-string obj)))
    (error "")))

(defun gosh-eldoc--elisp->scheme-string (obj)
  (cond
   ((eq obj nil)
    "()")
   ((atom obj)
    obj)
   ((eq (car obj) 'quote)
    (let (ret)
      (mapc
       (lambda (x)
         (setq ret (cons (gosh-eldoc--elisp->scheme-string x) ret)))
       (cdr obj))
      (mapconcat 'gosh-eldoc--object->string ret " ")))
   (t
    (mapcar
     (lambda (x)
       (gosh-eldoc--elisp->scheme-string x))
     obj))))

(defun gosh-eldoc--find-and-print-strings (specs env highlight)
  (mapcar (lambda (spec)
            (gosh-eldoc--find-and-print-string spec env highlight))
          specs))

(defun gosh-eldoc--find-and-print-string (spec env highlight)
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
          (and (eq (car type) 'syntax)
               "syntax: ")
          (gosh-eldoc--sexp->string
           (cons (car spec)
                 (gosh-eldoc--canonicalize-order
                  (mapcar 'gosh-eldoc--base-type
                          (gosh-eldoc--translate-dot-cell
                           (cadr type)))))
           highlight)
          (and (consp (cddr type))
               (not (memq (caddr type) '(obj object)))
               (concat " => " (gosh-eldoc--sexp->string (caddr type))))))
        ((and (consp type) (eq (car type) 'special))
         (gosh-eldoc--sexp->string (car spec)))
        ((and (consp type) (eq (car type) 'string) (stringp (cdr type)))
         (concat
          "string: \""
          (gosh-eldoc--sexp->string (cdr type))
          "\""))
        ((and (consp type) (eq (car type) 'number) (stringp (cdr type)))
         (concat
          "number: "
          (gosh-eldoc--sexp->string (cdr type))))
        ((symbolp type)
         (let ((spec (gosh-env-lookup env type)))
           ;; ignore `max-lisp-eval-depth' error
           (or (gosh-eldoc--find-and-print-string spec env highlight)
               (format "some parameter or alias typed `%s'" type))))
        (t
         (gosh-eldoc--sexp->string type)))
       (if (and (not (nth 3 spec)) (nth 4 spec)) " - " "")
       (or (nth 4 spec) ""))))
   ((and (consp spec) (null (cdr spec)))
    "some of ( parameter | alias | dynamic loaded procedure )")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defmacro gosh-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun gosh-goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line line)))

(defvar gosh-obarray (make-vector (length obarray) nil))

(defun gosh-intern (name)
  (intern name gosh-obarray))

(defun gosh-intern-soft (name)
  (intern-soft name gosh-obarray))

(defun gosh-symbol-eq (symbol1 symbol2)
  (let ((name1 (symbol-name symbol1))
        (name2 (symbol-name symbol2)))
    (eq (gosh-intern name1) (gosh-intern name2))))

(defun gosh-intern-safe (obj)
  (cond
   ((stringp obj)
    (gosh-intern obj))
   ((symbolp obj)
    (gosh-intern (symbol-name obj)))
   ((numberp obj)
    (gosh-intern (number-to-string obj)))
   (t
    (error "Assert"))))

(defun gosh-symbol-memq (symbol list)
  (memq (gosh-intern-safe symbol) 
        (mapcar 'gosh-intern-safe list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alist, list utilities

(defun gosh-put-alist (key value alist)
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

(defun gosh-set-alist (symbol key value)
  "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE.

This function come from apel"
  (or (boundp symbol)
      (set symbol nil))
  (set symbol (gosh-put-alist key value (symbol-value symbol))))

(defun gosh-filter (pred env)
  (mapcar 'car
          (apply 'concatenate
                 'list
                 (mapcar (lambda (e) (remove-if-not pred e)) env))))

(defun gosh-append-map (proc init-ls)
  (if (null init-ls)
      '()
    (let* ((ls (reverse init-ls))
           (res (funcall proc (pop ls))))
      (while (consp ls)
        (setq res (append (funcall proc (pop ls)) res)))
      res)))

(defun gosh-flatten (ls)
  (cond
   ((consp ls) (cons (car ls) (gosh-flatten (cdr ls))))
   ((null ls) '())
   (t (list ls))))

(defun gosh-flat (ls)
  (cond
   ((null ls) '())
   ((consp ls) 
    (append
     (gosh-flat (car ls))
     (gosh-flat (cdr ls))))
   ((atom ls) (cons ls nil))))

(defun gosh-nth* (n ls)
  (while (and (consp ls) (> n 0))
    (setq n (- n 1)
          ls (cdr ls)))
  (and (consp ls) (car ls)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utilities

(defun gosh-string-starts-with (pref str)
  (let ((p-len (length pref))
        (s-len (length str)))
    (and (<= p-len s-len)
         (equal pref (substring str 0 p-len)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file utilities

(defun gosh-any-file-in-path (file path)
  (car (remove-if-not
        (lambda (dir) (file-exists-p (concat dir "/" file)))
        path)))

(defun gosh-directory-tree-files (init-dir &optional match)
  (when (file-directory-p init-dir)
    (let ((res '())
          (stack (list init-dir)))
      (while (consp stack)
        (let* ((dir (pop stack))
               (files (cddr (directory-files dir))))
          (mapc
           (lambda (file)
             (let ((filename (expand-file-name file dir)))
               (cond
                ((string-match "^\\." file))
                ((file-directory-p filename)
                 (push filename stack))
                ((and match (string-match match file))
                 (setq res (cons filename res))))))
           files)))
      res)))

(defmacro gosh-with-find-file (path-expr &rest body)
  (declare (indent 1))
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

(defun gosh-file->lines (file)
  (and (file-readable-p file)
       (gosh-with-find-file file
         (goto-char (point-min))
         (let ((res '()))
           (while (not (eobp))
             (let ((start (point)))
               (forward-line)
               (push (buffer-substring-no-properties start (- (point) 1))
                     res)))
           (reverse res)))))

(defun gosh-file-mtime (file)
  (nth 5 (file-attributes file)))

;;
;; ui
;;

(defvar gosh-momentary-message-overlay nil)

(defun gosh-momentary-message (msg)
  "Show temporary message to current point.
referenced mew-complete.el"
  (let ((wait-msec (max (* (length msg) 0.05) 0.5))
        (modified (buffer-modified-p))
        (inhibit-read-only t)
        (buffer-undo-list t)
        start end)
    (save-excursion
      (setq start (point))
      (insert (concat " " msg))
      (set-buffer-modified-p modified)
      (setq end (point)))
    (let ((inhibit-quit t))
      (gosh-momentary-message-overlay start end)
      (sit-for wait-msec)
      (delete-region start end)
      (set-buffer-modified-p modified)
      (delete-overlay gosh-momentary-message-overlay)
      (when quit-flag
        (setq quit-flag nil)
        (setq unread-command-events (list 7))))))

(defun gosh-momentary-message-overlay (start end)
  (let ((ov gosh-momentary-message-overlay))
    (unless ov
      (setq ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face 'gosh-momentary-message-face)
    (move-overlay ov start end (current-buffer))
    (setq gosh-momentary-message-overlay ov)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse buffer

(defun gosh-context-string-p (&optional point)
  (save-excursion
    (let ((parses (parse-partial-sexp (point-min) (or point (point)))))
      (nth 3 parses))))

(defun gosh-context-comment-p (&optional point)
  (save-excursion
    (let ((parses (parse-partial-sexp (point-min) (or point (point)))))
      (nth 4 parses))))

(defun gosh-context-code-p (&optional point)
  (save-excursion
    (let ((parses (parse-partial-sexp (point-min) (or point (point)))))
      (and (not (nth 3 parses))
           (not (nth 4 parses))))))

(defun gosh-beginning-of-sexp ()
  (unless (bobp)
    (let ((syn (char-syntax (char-before (point)))))
      (if (or (eq syn ?\()
              (and (eq syn ?\") (gosh-context-string-p)))
          (forward-char -1)
        (forward-sexp -1)))))

;; returns current argument position within sexp
(defun gosh-beginning-of-current-sexp-operator ()
  (let ((pos 0))
    (skip-syntax-backward "w_")
    (while (and (not (bobp)) (not (eq ?\( (char-before))))
      (gosh-beginning-of-sexp)
      (incf pos))
    pos))

(defun gosh-beginning-of-next-sexp ()
  (forward-sexp 2)
  (backward-sexp 1))

(defun gosh-beginning-of-list ()
  (let ((first (point))
        top)
    (gosh-beginning-of-string)
    (while (and (not (setq top (looking-at "^(")))
                (not (bobp))
                (condition-case err
                    (progn 
                      (backward-sexp)
                      (skip-syntax-backward " "))
                  (scan-error nil))))
    (unless (or (bobp) top)
      (backward-char))
    (not (eq first (point)))))

(defun gosh-beginning-of-string ()
  (when (gosh-context-string-p)
    (search-backward "\"" nil t)
    ;; search double-quote non escaped
    (while (and (> (point) (point-min)) (eq ?\\ (char-before)))
      (search-backward "\"" nil t))))

(defun gosh-end-of-string ()
  (when (gosh-context-string-p)
    (search-forward "\"" nil t)
    ;; search double-quote non escaped
    (while (and (< (point) (point-max)) (eq ?\\ (char-before (1- (point)))))
      (search-forward "\"" nil t))))

;; for the enclosing sexp, returns a cons of the leading symbol (if
;; any) and the current position within the sexp (starting at 0)
;; (defun gosh-enclosing-sexp-prefix ()
;;   (save-excursion
;;     (let ((pos (gosh-beginning-of-current-sexp-operator)))
;;       (cons (gosh-parse-symbol-at-point) pos))))

(defun gosh-enclosing-2-sexp-prefixes ()
  (save-excursion
    (let* ((pos1 (gosh-beginning-of-current-sexp-operator))
           (sym1 (gosh-parse-symbol-at-point)))
      (backward-char)
      (or
       (ignore-errors
         (let ((pos2 (gosh-beginning-of-current-sexp-operator)))
           (list sym1 pos1 (gosh-parse-symbol-at-point) pos2)))
       (list sym1 pos1 nil 0)))))

(defun gosh-read ()
  (let ((start (point))
        end)
    (forward-sexp)
    (setq end (point))
    (let ((parsed (gosh-read-from-string (buffer-substring start end))))
      (when parsed
        (car parsed)))))

(defun gosh-read-from-string (string)
  (condition-case err
      (read-from-string string)
    (invalid-read-syntax 
     (if (string= (cadr err) "#")
         ;;FIXME not concern about in string
         ;; Probablly allmost case is ok.
         ;; Currently read scheme code only used as assistance of display.
         (gosh-hack-read-from-string string)
       ;; raise error as is
       (signal (car err) (cdr err))))))

(defun gosh-hack-read-from-string (string)
  (read-from-string
   (replace-regexp-in-string 
    "#" "\\\\#" 
    (gosh-hack-string-replace->readable-regexp string))))

(defun gosh-hack-string-replace->readable-regexp (string)
  (condition-case nil
      (let ((str string)
            before after reg ret)
        (while (string-match gosh-regexp-literal-regexp str)
          (setq reg (match-string 3 str))
          (setq before (substring str 0 (match-beginning 3)))
          (setq after (substring str (match-end 3)))
          (setq ret (concat ret before (gosh-hack-string-regexp->readable-string reg)))
          (setq str after))
        (concat ret str))
    (error
     "SOME-REGEXP")))

(defun gosh-hack-string-regexp->readable-string (regex)
  (let* ((reg regex)
         c before after ret)
    (while (string-match "^\\(\\\\.\\|.\\)" reg)
      (setq before (substring reg 0 (match-beginning 0)))
      (setq after  (substring reg (match-end 0)))
      (setq c (match-string 1 reg))
      (setq ret (concat 
                 ret before
                 (if (and (= (length c) 1) 
                          (memq (string-to-char c) '(?\[ ?\] ?\s ?\t ?\n)))
                     (concat "\\" c)
                   c)))
      (setq reg after))
    (concat ret reg)))

(defun gosh-goto-next-top-level ()
  (let ((here (point)))
    (or (ignore-errors (end-of-defun) (end-of-defun)
                       (beginning-of-defun)
                       (< here (point)))
        (progn (forward-char)
               (and (re-search-forward "^(" nil t)
                    (progn (backward-char 1) t)))
        (goto-char (point-max)))))

;; sexp-at-point is always fragile, both because the user can input
;; incomplete sexps and because some scheme sexps are not valid elisp
;; sexps.  this is one of the few places we use it, so we're careful
;; to wrap it in ignore-errors.
(defun gosh-nth-sexp-at-point (n)
  (ignore-errors
    (save-excursion
      (forward-sexp (+ n 1))
      (let ((end (point)))
        (forward-sexp -1)
        (car (gosh-read-from-string (buffer-substring (point) end)))))))

(defun gosh-parse-symbol-at-point ()
  (save-excursion
    (skip-syntax-backward "w_")
    (let ((start (point)))
      (skip-syntax-forward "w_")
      (and (< start (point))
           (intern (buffer-substring start (point)))))))

(defun gosh-parse-keyword-at-point ()
  (let ((sym (gosh-parse-symbol-at-point)))
    (when (and sym
               (keywordp sym))
      sym)))

(defun gosh-parse-context-toplevel-p ()
  (save-excursion
    (while (and (not (bobp))
                (condition-case err
                    (progn (backward-sexp) t)
                  (scan-error nil))))
    (bobp)))

(defun gosh-parse-last-expression-define-p ()
  "Return t if last expression is top-level define-*"
  (and (gosh-parse-context-toplevel-p)
       (save-excursion
         (beginning-of-defun)
         (looking-at "([ \t\n]*define"))))

(defun gosh-parse-sexp-type-at-point (&optional env)
  (case (char-syntax (char-after))
    ((?\()
     (forward-char 1)
     (when (eq ?w (char-syntax (char-after)))
       (let ((op (gosh-parse-symbol-at-point)))
         (cond
          ((eq op 'lambda)
           (let ((params
                  (gosh-nth-sexp-at-point 1)))
             `(lambda ,params)))
          (t
           (let ((spec (gosh-env-lookup env op)))
             (and spec
                  (consp (cadr spec))
                  (eq 'lambda (caadr spec))
                  (cddadr spec)
                  (car (cddadr spec)))))))))
    ((?\")
     (cons 'string (read (current-buffer))))
    ((?\w)
     (if (string-match "[0-9]" (string (char-after)))
         ;; To avoid overflow to string
         (cons 'number (symbol-name (gosh-parse-symbol-at-point)))
       (gosh-parse-symbol-at-point)))
    (t
     nil)))

(defun gosh-parse-let-vars-at-point (&optional env limit loopp)
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
          (when (and loopp (looking-at "\\(for\\|let\\|with\\)\\>"))
            (gosh-beginning-of-next-sexp))
          (when (eq ?w (char-syntax (char-after)))
            (let* ((sym (gosh-parse-symbol-at-point))
                   (type (and (not loopp)
                              (ignore-errors
                                (gosh-beginning-of-next-sexp)
                                (gosh-parse-sexp-type-at-point env)))))
              (push (if type (list sym type) (list sym)) vars)))
          (when loopp
            (while (and (< (point) end)
                        (ignore-errors
                          (gosh-beginning-of-next-sexp)
                          (eq ?w (char-syntax (char-after)))))
              (push (list (gosh-parse-symbol-at-point)) vars)))))
      (unless (ignore-errors 
                (let ((here (point)))
                  (gosh-beginning-of-next-sexp)
                  (> (point) here)))
        (goto-char end)))
    (reverse vars)))

(defun gosh-parse-extract-match-clause-vars (x)
  (cond
   ((null x) '())
   ((symbolp x)
    (if (memq x '(_ ___ \.\.\.))
        '()
      (list (list x))))
   ((consp x)
    (case (car x)
      ((or not)
       (gosh-parse-extract-match-clause-vars (cdr x)))
      ((and)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (consp (caddr x))
                (not (memq (caaddr x)
                           '(= $ @ ? and or not quote quasiquote get! set!))))
           (cons (list (cadr x) (if (listp (caddr x)) 'list 'pair))
                 (gosh-parse-extract-match-clause-vars (cddr x)))
         (gosh-parse-extract-match-clause-vars (cddr x))))
      ((= $ @)
       (if (consp (cdr x)) (gosh-parse-extract-match-clause-vars (cddr x)) '()))
      ((\? ? ) ; XXXX this is a hack, the lone ? gets read as a char (space)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (symbolp (caddr x)))
           (cons (list (caddr x) (gosh-predicate->type (cadr x)))
                 (gosh-parse-extract-match-clause-vars (cdddr x)))
         (gosh-parse-extract-match-clause-vars (cddr x))))
      ((get! set!)
       (if (consp (cdr x)) (gosh-parse-extract-match-clause-vars (cadr x)) '()))
      ((quote) '())
      ((quasiquote) '())                ; XXXX
      (t
       (union (gosh-parse-extract-match-clause-vars (car x))
              (gosh-parse-extract-match-clause-vars (cdr x))))))
   ((vectorp x)
    (gosh-parse-extract-match-clause-vars (concatenate 'list x)))
   (t
    '())))

;; call this from the first opening paren of the match clauses
(defun gosh-parse-extract-match-vars (&optional pos limit)
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
            (let* ((pat (gosh-nth-sexp-at-point 0))
                   (new-vars (ignore-errors
                               (gosh-parse-extract-match-clause-vars pat))))
              (setq match-vars (append new-vars match-vars)))
            (goto-char (if (or pos (not end)) limit (+ end 1)))))))
      match-vars)))

(defun gosh-parse-current-env ()
  ;; r5rs
  (let ((env (list *gosh-scheme-r5rs-info*)))
    ;; base language
    (let ((base *gosh-documented-exports*))
      (when base
        (push base env)))
    ;; imports
    (let ((imports (ignore-errors (gosh-parse-current-import-symbols))))
      (when imports
        (push imports env)))
    ;; top-level defs
    (let ((top (ignore-errors (gosh-parse-current-globals))))
      (when top
        (push top env)))
    ;; current local vars
    (let ((locals (ignore-errors (gosh-parse-context-local-vars env))))
      (when locals
        (push locals env)))
    env))

(defun gosh-parse-context-module ()
  (save-excursion
    (save-restriction
      (widen)
      (catch 'return
        ;; avoid starts of "(with-module"
        (when (looking-at "(")
          (unless (bobp)
            (backward-char)))
        (let ((matcher (lambda ()
                         (when (looking-at "(\\(?:with\\|define\\)-module[ \t\n]+\\([^ \t\n()]+\\)")
                           (throw 'return (match-string-no-properties 1)))))
              (toplevel (gosh-parse-context-toplevel-p)))
          (unless toplevel
            (while (and (not (bobp))
                        (not (looking-at "^("))) ; while not top level
              (funcall matcher)
              (gosh-beginning-of-list))
            (funcall matcher)))
        (when (re-search-backward "^[ \t]*(select-module[ \t\n]+\\([^ \t\n()]+\\)" nil t)
          (throw 'return (match-string-no-properties 1)))
        "user"))))

(defun gosh-parse-buffer-import-modules ()
  (mapcar 'car (gosh-parse-buffer-import-modules-with-prefix)))

;; use or import
(defun gosh-parse-buffer-import-modules-with-prefix ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((res '()))
        (while (re-search-forward "(\\(?:use\\|import\\)[ \t\n]" nil t)
          (when (gosh-context-code-p)
            (ignore-errors
              (save-excursion
                (goto-char (match-beginning 0))
                (let* ((sexp (gosh-read))
                       (module (cadr sexp))
                       (pref (memq :prefix sexp)))
                  (setq res (cons 
                             (cons module 
                                   (if (and pref (symbolp (cadr pref)))
                                       (symbol-name (cadr pref))
                                     ""))
                             res)))))))
        (nreverse res)))))

(defun gosh-parse-context-local-vars (&optional env)
  (let ((vars '())
        (start (point))
        (limit (save-excursion (beginning-of-defun) (+ (point) 1)))
        (let-limit (save-excursion (gosh-beginning-of-sexp)
                                   (gosh-beginning-of-sexp)
                                   (point)))
        (scan-internal))
    (save-excursion
      (while (> (point) limit)
        (or (ignore-errors
              (progn
                (skip-chars-backward " \t\n" limit)
                (gosh-beginning-of-sexp)
                t))
            (goto-char limit))
        (when (and (> (point) (point-min))
                   (eq ?\( (char-syntax (char-before (point))))
                   (eq ?w (char-syntax (char-after (point)))))
          (setq scan-internal t)
          (let ((sym (gosh-parse-symbol-at-point)))
            (case sym
              ((lambda)
               (setq vars
                     (append
                      (mapcar 'list
                              (gosh-flatten (gosh-nth-sexp-at-point 1)))
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
                            (when (eq sym 'match)
                              (forward-sexp 1))
                            (backward-sexp 1)
                            (when (not (eq sym 'match))
                              (forward-char 1))
                            (gosh-parse-extract-match-vars
                             (and (or (eq sym 'match) (< start limit)) start)
                             limit))))
                      vars)))
              ((let let* letrec letrec* let-syntax letrec-syntax
                    and-let* do loop)
               (or
                (ignore-errors
                  (save-excursion
                    (gosh-beginning-of-next-sexp)
                    (let* ((loop-name
                            (and (memq sym '(let loop))
                                 (eq ?w (char-syntax (char-after (point))))
                                 (prog1 (gosh-parse-symbol-at-point)
                                   (gosh-beginning-of-next-sexp))))
                           (let-vars
                            (gosh-parse-let-vars-at-point
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
                              (gosh-append-map
                               'gosh-flatten
                               (remove-if-not 'consp
                                              (gosh-nth-sexp-at-point 1))))
                             vars)))
              ((receive defun defmacro)
               (setq vars
                     (append (mapcar 'list
                                     (gosh-flatten
                                      (gosh-nth-sexp-at-point 1)))
                             vars)))
              (t
               (if (string-match "^define\\(-.*\\)?" (symbol-name sym))
                   (let ((defs (save-excursion
                                 (backward-char)
                                 (gosh-parse-read-definitions))))
                     (setq vars
                           (append (gosh-append-map
                                    (lambda (x)
                                      (and (consp (cdr x))
                                           (consp (cadr x))
                                           (eq 'lambda (caadr x))
                                           (mapcar 'list
                                                   (gosh-flatten
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
                  (when (< (point) start)
                    (setq vars (append (gosh-parse-read-trailing-definitions) vars))))))))))
    (reverse vars)))

(defun gosh-parse-sexp-imports (sexp)
  (case (and (consp sexp) (car sexp))
    ((begin define-module)
     (gosh-append-map 'gosh-parse-sexp-imports (cdr sexp)))
    ((cond-expand)
     (gosh-append-map 'gosh-parse-sexp-imports
                      (gosh-append-map 'cdr (cdr sexp))))
    ((use import)
     (let* ((prefix (cadr (memq :prefix sexp)))
            (pref (or (and prefix (symbolp prefix) (symbol-name prefix))
                      (and (stringp prefix) prefix)
                      "")))
       (mapcar
        (lambda (d)
          (if (symbolp (car d))
              (cons (intern (concat pref (symbol-name (car d)))) (cdr d))
            d))
        (gosh-module-exports-definitions (cadr sexp)))))
    ((require-extension)
     (gosh-append-map 'gosh-parse-sexp-imports (cdr sexp)))
    ((autoload)
     ;;TODO only arg is symbol
     (mapcar (lambda (x) (cons (if (consp x) (car x) x) '((lambda obj))))
             (cddr sexp)))))

(defun gosh-parse-sexp-import-modules (sexp)
  (case (and (consp sexp) (car sexp))
    ((define-module)
     (gosh-append-map 'gosh-parse-sexp-import-modules (cdr sexp)))
    ((use)
     (list (cadr sexp)))
    ((require-extension)
     (when (eq (cadr sexp) 'srfi)
       (remove 
        nil
        (mapcar (lambda (n) 
                  (when (numberp n)
                    (intern (concat "srfi-" (number-to-string n)))))
               (cddr sexp)))))
    ((import)
     (cdr sexp))))

(defun gosh-module-symbol-p (sym)
  (memq sym '(use require require-extension begin cond-expand
                  define-module autoload import)))

(defun gosh-skip-shebang ()
  ;; skip shebang if present
  (if (looking-at "#!")
      (forward-line)))

(defun gosh-parse-fnsym-current-sexp ()
  (let (sexp count count-up dangling)
    (unless (gosh-parse-context-toplevel-p)
      (save-excursion
        (let (start end)
          (gosh-beginning-of-string)
          (unless (setq dangling
                        (or (looking-at "[ \n\t]*[()]")
                            (looking-at "[ \n\t]")))
            (condition-case err
                (forward-sexp)
              (scan-error 
               (setq count-up t))))
          (setq end (point))
          (gosh-beginning-of-list)
          ;; simply ignore this context
          (unless (looking-at "\\[")
            (setq start (point))
            (ignore-errors
              (setq sexp (car
                          (gosh-read-from-string 
                           (concat (buffer-substring start end) ")"))))))
          (cond
           ((not (consp sexp))
            (setq sexp (cons nil nil)))
           ((not (symbolp (car sexp)))
            (setq sexp (cons nil (cdr sexp))))))))
    (condition-case err
        (setq count (length (cdr sexp)))
      ;; cons cell make error but ignore.
      (error (setq count 0
                   sexp nil)))
    (when (or (and dangling
                   (memq (char-syntax (char-before (point))) '(?\s ?\>)))
              count-up)
      (setq count (1+ count)))
    (list sexp count)))

(defun gosh-parse-current-import-symbols ()
  (let ((res '()))
    (save-excursion
      (goto-char (point-min))
      (gosh-skip-shebang)
      ;; scan for module forms
      (while (not (eobp))
        (if (ignore-errors (forward-sexp) t)
            (let ((end (point)))
              (backward-sexp)
              (when (eq ?\( (char-after))
                (forward-char)
                (when (not (eq ?\( (char-after)))
                  (let ((sym (gosh-parse-symbol-at-point)))
                    (cond
                     ((gosh-module-symbol-p sym)
                      (backward-char)
                      (ignore-errors
                        (setq res
                              (append (gosh-parse-sexp-imports
                                       (gosh-nth-sexp-at-point 0))
                                      res))))))))
              (goto-char end))
          ;; if an incomplete sexp is found, try to recover at the
          ;; next line beginning with an open paren
          (gosh-goto-next-top-level))))
    res))

;; we should be just inside the opening paren of an expression
(defun gosh-parse-name-of-current-define ()
  (save-excursion
    (gosh-beginning-of-next-sexp)
    (if (eq ?\( (char-syntax (char-after)))
        (forward-char))
    (and (memq (char-syntax (char-after)) '(?\w ?\_))
         (gosh-parse-symbol-at-point))))

(defun gosh-parse-type-of-current-define ()
  (save-excursion
    (gosh-beginning-of-next-sexp)
    (cond
     ((eq ?\( (char-syntax (char-after)))
      `(lambda ,(cdr (gosh-nth-sexp-at-point 0))))
     (t
      (ignore-errors 
        (gosh-beginning-of-next-sexp)
        (gosh-parse-sexp-type-at-point))))))

(defun gosh-parse-applying-of-current-define ()
  (save-excursion
    (gosh-beginning-of-next-sexp)
    (gosh-beginning-of-next-sexp)
    (ignore-errors 
      `(lambda ,(gosh-nth-sexp-at-point 0)))))

;; we should be at the opening paren of an expression
(defun gosh-parse-read-definitions (&optional env)
  (save-excursion
    (let ((sym (ignore-errors 
                 (and (eq ?\( (char-syntax (char-after)))
                      (progn (forward-char)
                             (gosh-parse-symbol-at-point))))))
      (case sym
        ((define-syntax define-compiled-syntax defmacro define-macro)
         (list (list (gosh-parse-name-of-current-define) '(syntax))))
        ((define-method)
         (let ((name (gosh-parse-name-of-current-define))
               (type (gosh-parse-applying-of-current-define)))
           (list (if type (list name type) (list name)))))
        ((define define-constant)
         (let ((name (gosh-parse-name-of-current-define))
               (type (gosh-parse-type-of-current-define)))
           (list (if type (list name type) (list name)))))
        ((define-class)
         (list (list (gosh-parse-name-of-current-define) 'non-procedure)))
        ((define-record-type)
         (backward-char)
         (ignore-errors
           (let ((sexp (gosh-nth-sexp-at-point 0)))
             `((,(caaddr sexp) (lambda ,(cdaddr sexp)))
               (,(cadddr sexp) (lambda (obj)))
               ,@(gosh-append-map 
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
         (gosh-parse-read-trailing-definitions))
        (t
         '())))))

(defun gosh-scheme-in-defun-name ()
  (save-excursion
    (gosh-beginning-of-list)
    (and (gosh-beginning-of-list)
         (looking-at "^(define")
         (point))))

(defun gosh-parse-file-globals (file)
  (gosh-with-find-file file
    (save-excursion
      (goto-char (point-min))
      (gosh-parse-current-globals t))))

(defun gosh-parse-file-exports (file)
  (let (syms modules)
    (gosh-with-find-file file
      (setq syms (gosh-parse-exported-symbols))
      (setq modules (gosh-parse-current-base-modules)))
    (mapc
     (lambda (mod)
       (let ((f (gosh--module->file mod)))
         (setq syms (append (gosh-parse-file-exports f) syms))))
     modules)
    syms))

;; for internal defines, etc.
(defun gosh-parse-read-trailing-definitions (&optional enclosing-end)
  (let ((defs '())
        (end (or enclosing-end (point-max))))
    (save-excursion
      (while (< (point) end)
        (let ((here (point))
              (new-defs (gosh-parse-read-definitions)))
          (cond
           (new-defs
            (setq defs (append new-defs defs))
            (or (ignore-errors 
                  (gosh-beginning-of-next-sexp)
                  (> (point) here))
                (goto-char end)))
           ;; non-definition form, maybe stop scanning
           (t
            (goto-char end))))))
    defs))

(defun gosh-parse-file-base-modules (file)
  (gosh-with-find-file file
    (gosh-parse-current-base-modules)))

(defun gosh-parse-current-base-modules ()
  (let (mod modules)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(extend\\b" nil t)
        (when (gosh-context-code-p)
          (ignore-errors
            (while (setq mod (read (current-buffer)))
              (setq modules (cons mod modules)))))))
    (nreverse modules)))

(defun gosh-parse-exported-symbols ()
  (let ((env (gosh-parse-current-globals))
        (exports (gosh-parse-current-exports t))
        (res '()))
    ;; if source file execute dynamic load.
    ;; global definition (env) will be null.
    (mapc
     (lambda (x)
       (setq res
             (cons (or (assq x env)
                       (cons x nil))
                   res)))
     exports)
    ;; merge generic functions
    (mapc
     (lambda (x)
       (when (memq (car x) exports)
         (unless (memq x res)
           (setq res (cons x res)))))
     env)
    res))

(defun gosh-parse-current-toplevel-symbol ()
  (when (and (eq ?\( (char-syntax (char-after)))
             (eq ?w (char-syntax (char-after (1+ (point))))))
    (save-excursion 
      (forward-char) 
      (gosh-parse-symbol-at-point))))

;; a little more liberal than gosh-parse-read-trailing-definitions we try to scan to a new
;; top-level form (i.e. a line beginning with an open paren) if
;; there's an error during normal sexp movement
(defun gosh-parse-current-globals (&optional only-current)
  (let ((res '()))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (or (ignore-errors (end-of-defun) (backward-sexp) t)
            (and (re-search-forward "^(" nil t) (progn (backward-char) t))
            (goto-char (point-max)))
        (while (not (eobp))
          (let ((sym (gosh-parse-current-toplevel-symbol)))
            (case sym
              ((define-module)
               (unless only-current
                 (let* ((decls (gosh-nth-sexp-at-point 0))
                        (parents (cdr (assq 'extend decls))))
                   (setq res (append
                              (gosh-append-map
                               'gosh-module-global-definitions
                               parents)
                              res)))))
              ((extend)
               (unless only-current
                 (let ((parents (cdr (gosh-nth-sexp-at-point 0))))
                   (setq res (append
                              (gosh-append-map
                               'gosh-module-global-definitions
                               parents)
                              res)))))))
          (setq res
                (append
                 (ignore-errors (gosh-parse-read-definitions))
                 res))
          (or (and (progn (forward-char) (re-search-forward "^(" nil t))
                   (progn (backward-char) t))
              (gosh-goto-next-top-level)))))
    res))

(defun gosh-parse-current-exports (&optional only-current)
  (let ((res '()))
    (save-excursion
      (goto-char (point-min))
      (or (ignore-errors (end-of-defun) (beginning-of-defun) t)
          (re-search-forward "^(" nil t)
          (goto-char (point-max)))
      (while (not (eobp))
        (let ((sym (gosh-parse-current-toplevel-symbol)))
          (case sym
            ((define-module)
             (let ((decls (gosh-nth-sexp-at-point 0)))
               (unless only-current
                 (let ((parents (cdr (assq 'extend decls))))
                   (setq res (append
                              (mapcar 'car
                                      (gosh-append-map
                                       'gosh-module-exports-definitions
                                       parents))
                              res))))
               (cond
                ((and (listp decls) (assq 'export decls))
                 (setq res (nconc (cdr (assq 'export decls)) res)))
                ((and (listp decls) (assq 'export-all decls))
                 (goto-char (point-max))))))
            ((export export-if-defined)
             (setq res (nconc (cdr (gosh-nth-sexp-at-point 0)) res)))
            ((export-all)
             (goto-char (point-max)))
            ((extend)
             (unless only-current
               (let ((parents (cdr (gosh-nth-sexp-at-point 0))))
                 (setq res (append
                            (mapcar 'car
                                    (gosh-append-map
                                     'gosh-module-exports-definitions
                                     parents))
                            res)))))))
        (gosh-goto-next-top-level)))
    res))

(defun gosh-scheme-srfi-exports (i)
  (and (integerp i)
       (>= i 0)
       (< i (length *gosh-scheme-srfi-info*))
       (let ((info (cdr (aref *gosh-scheme-srfi-info* i))))
         (if (and (consp info) (null (cdr info)) (symbolp (car info)))
             (gosh-module-exports-definitions (car info))
           info))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsed cache


;;TODO to have extend relation
(defvar gosh-cache-module-exports '()
  "Each item is following list
\\(file-name module-symbol time symbols-alist [defined-symbols])

TODO key should be module-file?? multiple executable make complex.
"
  )

;;TODO to have extend relation
;;TODO refactor file information collect to one object. same *-module-exports
(defvar gosh-cache-file-globals '()
  "Each item is following list
\\(file-name module-symbol time symbols-alist)"
  )

(defun gosh-module-exports-definitions (mod)
  (cond
   ((and (consp mod) (eq 'srfi (car mod)))
    (gosh-append-map 'gosh-scheme-srfi-exports (cdr mod)))
   ((and (symbolp mod) (string-match "^srfi-[0-9]+$" (symbol-name mod)))
    (gosh-scheme-srfi-exports
     (string-to-number (substring (symbol-name mod) 5))))
   (t
    (let* ((file (gosh--module->file mod))
           (cached (gosh-cache-find-by-file file 'gosh-cache-module-exports)))
      (if cached
          (nth 3 cached)
        ;; (re)compute module exports
        (let ((res (gosh-parse-file-exports file)))
          (push (list file mod (gosh-file-mtime file) res)
                gosh-cache-module-exports)
          res))))))

(defun gosh-module-global-definitions (mod)
  (let* ((file (gosh--module->file mod))
         (cached (gosh-cache-find-by-file file 'gosh-cache-file-globals)))
    (if cached
        (nth 4 cached)
      ;; (re)compute module exports
      (let ((res (gosh-parse-file-globals file))
            (bases (gosh-parse-file-base-modules file)))
        (push (list file mod (gosh-file-mtime file) bases res)
              gosh-cache-file-globals)
        res))))

(defun gosh-cache-find-by-file (file cache-var)
  (let* ((predicate (lambda (item) 
                      (equal (nth 0 item) file)))
         (cached (find-if predicate (symbol-value cache-var))))
    (when (and cached
               (stringp (nth 0 cached))
               (ignore-errors
                 (let ((mtime (gosh-file-mtime (nth 0 cached)))
                       (ctime (nth 2 cached)))
                   (not (or (equal mtime ctime)
                            (time-less-p mtime ctime))))))
      (set cache-var (delete-if predicate (symbol-value cache-var)))
      (setq cached nil))
    cached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For cygwin path 

(defcustom gosh-cygwin-cygdrive "/cygdrive/"
  "Path alias of Windows drive prefixed path in Cygwin.

c:/Windows == /cygdrive/c/Windows
d:/home == /cygdrive/d/home
"
  :group 'gosh-mode
  :type 'string
  :set (lambda (s v) (set s (file-name-as-directory v)))
  :initialize (lambda (s v) (set s v)))

(defcustom gosh-cygwin-directory "c:/cygwin/"
  "Cygwin installed directory."
  :group 'gosh-mode
  :type 'directory
  :set (lambda (s v) (set s (file-name-as-directory (expand-file-name v))))
  :initialize (lambda (s v) (set s v)))

(defun gosh-cygpath->emacs-path (path)
  (let ((cygdrive gosh-cygwin-cygdrive)
        (installed gosh-cygwin-directory)
        (case-fold-search t))
    (cond
     ((string-match (format "^\\(?:%s\\)\\([a-zA-Z]\\)/\\(.*\\)" (regexp-quote cygdrive)) path)
      (format "%s:/%s" (match-string 1 path) (match-string 2 path)))
     ((string-match "^/" path)
      (expand-file-name (substring path 1) installed))
     (t
      path))))

(defun gosh-emacs-path->cygpath (path)
  (let ((path (expand-file-name path))
        (cygdrive gosh-cygwin-cygdrive)
        (installed gosh-cygwin-directory)
        (case-fold-search t))
    (cond
     ((string-match (concat "^" (regexp-quote installed) "\\(.*\\)") path)
      (concat "/" (match-string 1 path)))
     ((string-match "^\\([a-zA-Z]\\):/\\(.*\\)" path)
      (format "%s%s/%s" cygdrive (match-string 1 path) (match-string 2 path)))
     ((string-match "^/" path)
      (expand-file-name (substring path 1) installed))
     (t
      path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is rather complicated because we want to auto-generate
;; docstring summaries from the type information, which means
;; inferring various types from common names.  The benefit is that you
;; don't have to input the same information twice, and can often
;; cut&paste&munge procedure descriptions from the original
;; documentation.

(defun gosh-scheme-translate-type (type)
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
             (gosh-scheme-translate-type (intern (substring str 0 i)))
           (let ((i (string-match "-\\([^-]+\\)$" str)))
             (if i
                 (gosh-scheme-translate-type (intern (substring str (+ i 1))))
               (if (string-match "\\?$" str)
                   'boolean
                 'object)))))))))

(defun gosh-lookup-type (spec pos)
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
        (setq type (gosh-scheme-translate-type type)))
    type))

(defun gosh-predicate->type (pred)
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
           (gosh-scheme-translate-type
            (intern (substring str 0 (- (length str) 1))))
         'object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion

(defun gosh-scheme-do-completion (str coll &optional strs pred)
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
     ((eq completion t)
      (gosh-momentary-message "[Sole completion]"))
     ((not completion)
      (ding)
      (gosh-momentary-message "[No completion]"))
     ((not (string= str completion))
      (let ((all (all-completions str (append strs coll) pred))
            (prefix-p (gosh-string-starts-with completion completion1)))
        (unless prefix-p
          (save-excursion
            (backward-char (length str))
            (insert "\"")))
        (insert (substring completion (length str)))
        (unless prefix-p
          (insert "\"")
          (backward-char))
        (if (= (length all) 1)
            (gosh-momentary-message "[Sole completion]")
          (gosh-momentary-message "[Type again to show completions]"))))
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
                  (let ((inhibit-quit t))
                    (read-event))))
            (cond
             ((memq orig-event '(tab 9))
              (save-selected-window
                (select-window (get-buffer-window "*Completions*"))
                (if (pos-visible-in-window-p (point-max))
                    (goto-char (point-min))
                  (scroll-up))))
             (t
              (set-window-configuration win-config)
              (if (memq orig-event '(space 32))
                  (bury-buffer (get-buffer "*Completions*"))
                (setq unread-command-events (list orig-event)))
              (setq done t))))))))))

(defun gosh-env-matches (env sym with-detailed)
  (let ((specs nil)
        (ls env)
        cands l tmp)
    (while ls
      (setq l (pop ls))
      (while (setq tmp (assq sym l))
        (setq l (cdr (memq tmp l)))
        (setq cands (cons tmp cands))
        (when (or (null with-detailed)
                  (cdr tmp))
          (setq specs (cons tmp specs)))))
    ;; when specs are not found if with-detailed or not,
    ;; return first candidate.
    (unless specs
      (when (consp cands)
        (setq specs (list (car cands)))))
    specs))

(defun gosh-env-lookup (env sym)
  (let ((specs (gosh-env-matches env sym nil))
        spec tmp)
    ;; find most detailed info
    (while specs
      (setq tmp (pop specs))
      (when (and (consp tmp)
                 (or (> (length tmp) (length spec))
                     (and (= (length tmp) (length spec))
                          ;;FIXME TODO....
                          (consp (cadr tmp)) 
                          (consp (cadr spec))
                          (eq 'lambda (caadr tmp))
                          (eq 'lambda (caadr spec))
                          ;; compare lambda args
                          (> (length (gosh-flat (cadadr tmp))) 
                             (length (gosh-flat (cadadr spec)))))))
        (setq spec tmp)))
    spec))

;; checking return values:
;;   a should be capable of returning instances of b
(defun gosh-type-match-p (a b)
  (let ((a1 (gosh-scheme-translate-type a))
        (b1 (gosh-scheme-translate-type b)))
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
                           (gosh-type-match-p
                            a1 (gosh-scheme-translate-type x)))
                         (cdr b1))
                      (let ((b2 (gosh-translate-special-type b1)))
                        (and (not (equal b1 b2))
                             (gosh-type-match-p a1 b2))))))
               (and (consp a1)
                    (case (car a1)
                      ((or)
                       ;; type unions
                       (find-if
                        (lambda (x)
                          (gosh-type-match-p (gosh-scheme-translate-type x) b1))
                        (cdr a1)))
                      ((lambda)
                       ;; procedures
                       (or (eq 'procedure b1)
                           (and (consp b1)
                                (eq 'lambda (car b1))
                                (gosh-param-list-match-p (cadr a1)
                                                         (cadr b1)))))
                      (t
                       ;; other special types
                       (let ((a2 (gosh-translate-special-type a1))
                             (b2 (gosh-translate-special-type b1)))
                         (and (or (not (equal a1 a2)) (not (equal b1 b2)))
                              (gosh-type-match-p a2 b2)))))))))))

(defun gosh-param-list-match-p (p1 p2)
  (or (and (symbolp p1) (not (null p1)))
      (and (symbolp p2) (not (null p2)))
      (and (null p1) (null p2))
      (and (consp p1) (consp p2)
           (gosh-param-list-match-p (cdr p1) (cdr p2)))))

(defun gosh-translate-special-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((list string) (car x))
      ((set special) (cadr x))
      ((flags) 'integer)
      (t x))))

(defun gosh-cmoplete-symbol-name-at-point ()
  (let ((end (point))
        (start (save-excursion (skip-syntax-backward "w_") (point))))
    (buffer-substring-no-properties start end)))

(defun gosh-complete-file-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir "."))))
    (if dir
        (mapcar (lambda (f) (concat dir f)) res)
      res)))

(defun gosh-complete-directory-name (trans sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir ".")))
         (res2 (if dir (mapcar (lambda (f) (concat dir f)) res) res)))
    (remove-if-not 'file-directory-p res2)))

(defun gosh-string-completer (type)
  (case type
    ((filename)
     '(gosh-complete-file-name file-name-nondirectory))
    ((directory)
     '(gosh-complete-directory-name file-name-nondirectory))
    (t
     (cond
      ((and (consp type) (eq 'string (car type)))
       (cadr type))
      ((and (consp type) (eq 'or (car type)))
       (car (delete nil (mapcar 'gosh-string-completer (cdr type)))))))))

(defun gosh-apply-string-completer (cmpl sym)
  (let ((func (if (consp cmpl) (car cmpl) cmpl))
        (trans (and (consp cmpl) (cadr cmpl))))
    (funcall func trans sym)))

(defun gosh-smart-complete (&optional arg)
  (interactive "P")
  (let* ((sym (gosh-cmoplete-symbol-name-at-point))
         (in-str-p (gosh-context-string-p))
         (x (save-excursion
              (if in-str-p (gosh-beginning-of-string))
              (gosh-enclosing-2-sexp-prefixes)))
         (inner-proc (car x))
         (inner-pos (cadr x))
         (outer-proc (caddr x))
         (outer-pos (cadddr x))
         (env (save-excursion
                (if in-str-p (gosh-beginning-of-string))
                (gosh-parse-current-env)))
         (outer-spec (gosh-env-lookup env outer-proc))
         (outer-type (gosh-scheme-translate-type (cadr outer-spec)))
         (inner-spec (gosh-env-lookup env inner-proc))
         (inner-type (gosh-scheme-translate-type (cadr inner-spec))))
    (cond
     ;; return all env symbols when a prefix arg is given
     (arg
      (gosh-scheme-do-completion sym (gosh-filter (lambda (x) t) env)))
     ;; allow different types of strings
     (in-str-p
      (let* ((param-type
              (and (consp inner-type)
                   (eq 'lambda (car inner-type))
                   (gosh-lookup-type (cadr inner-type) inner-pos)))
             (completer (or (gosh-string-completer param-type)
                            '(gosh-complete-file-name
                              file-name-nondirectory))))
        (gosh-scheme-do-completion
         ;;(if (consp completer) (funcall (cadr completer) sym) sym)
         sym
         (gosh-apply-string-completer completer sym))))
     ;; outer special
     ((and (consp outer-type)
           (eq 'special (car outer-type))
           (cadddr outer-type))
      (gosh-scheme-do-completion sym (funcall (cadddr outer-type) sym)))
     ;; inner special
     ((and (consp inner-type)
           (eq 'special (car inner-type))
           (caddr inner-type))
      (gosh-scheme-do-completion sym (funcall (caddr inner-type) sym)))
     ;; completing inner procedure, complete procedures with a
     ;; matching return type
     ((and (consp outer-type)
           (eq 'lambda (car outer-type))
           (not (zerop outer-pos))
           (gosh-nth* (- outer-pos 1) (cadr outer-type))
           (or (zerop inner-pos)
               (and (>= 1 inner-pos)
                    (consp inner-type)
                    (eq 'lambda (car inner-type))
                    (let ((param-type
                           (gosh-lookup-type (cadr inner-type) inner-pos)))
                      (and (consp param-type)
                           (eq 'lambda (car param-type))
                           (eq (caddr inner-type) (caddr param-type)))))))
      (let ((want-type (gosh-lookup-type (cadr outer-type) outer-pos)))
        (gosh-scheme-do-completion
         sym
         (gosh-filter
          (lambda (x)
            (let ((type (cadr x)))
              (or (memq type '(procedure object nil))
                  (and (consp type)
                       (or (and (eq 'syntax (car type))
                                (not (eq 'undefined (caddr type))))
                           (and (eq 'lambda (car type))
                                (gosh-type-match-p (caddr type)
                                                   want-type)))))))
          env))))
     ;; completing a normal parameter
     ((and inner-proc
           (not (zerop inner-pos))
           (consp inner-type)
           (eq 'lambda (car inner-type)))
      (let* ((param-type (gosh-lookup-type (cadr inner-type) inner-pos))
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
                              (gosh-lookup-type (cadr outer-type)
                                                outer-pos)))
                         (and (consp outer-param-type)
                              (eq 'flags (car outer-param-type))
                              (memq (gosh-scheme-translate-type param-type)
                                    '(number complex real rational integer))
                              (memq (gosh-scheme-translate-type (caddr inner-type))
                                    '(number complex real rational integer))
                              (cdr outer-param-type))))))
             (base-type (if set-or-flags
                            (if (and (consp param-type)
                                     (eq 'set (car param-type)))
                                (gosh-scheme-translate-type (cadr param-type))
                              'integer)
                          param-type))
             (base-completions
              (gosh-filter
               (lambda (x)
                 (and (not (and (consp (cadr x)) (eq 'syntax (caadr x))))
                      (gosh-type-match-p (cadr x) base-type)))
               env))
             (str-completions
              (let ((completer (gosh-string-completer base-type)))
                (and
                 completer
                 (gosh-apply-string-completer completer sym)))))
        (gosh-scheme-do-completion
         sym
         (append set-or-flags base-completions)
         str-completions)))
     ;; completing a function
     ((zerop inner-pos)
      (gosh-scheme-do-completion
       sym
       (gosh-filter
        (lambda (x)
          (or (null (cdr x))
              (memq (cadr x) '(procedure object nil))
              (and (consp (cadr x))
                   (memq (caadr x) '(lambda syntax special)))))
        env)))
     ;; complete everything
     (t
      (gosh-scheme-do-completion sym (gosh-filter (lambda (x) t) env))))))

(defun gosh-complete-or-indent (&optional arg)
  (interactive "P")
  (let* ((end (point))
         (func
          (save-excursion
            (beginning-of-line)
            (if (re-search-forward "\\S-" end t)
                'gosh-smart-complete
              'lisp-indent-line))))
    (funcall func arg)))

;;
;; auto-complete
;;

;; Generic initialize function for auto-complete
(defun gosh-ac-initialize ()
  ;; only activate if usually using auto-complete
  (when (featurep 'auto-complete)
    ;; When compiling by `make',
    ;; auto-complete package is not known where to exist.
    (dont-compile
      (ac-define-source gosh-functions
        '((candidates . gosh-ac-function-candidates)
          (symbol . "f")
          (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
          (requires . 2)
          (cache)))

      (ac-define-source gosh-symbols
        '((candidates . gosh-ac-symbol-candidates)
          (symbol . "s")
          (prefix . "[ \t\n]\\(\\(?:\\sw\\|\\s_\\)+\\)")
          (requires . 2)
          (cache)))

      (ac-define-source gosh-keywords
        '((candidates . gosh-ac-keywords-candidates)
          (symbol . "k")
          (prefix . "[ \t\n]\\(:\\(?:\\sw\\|\\s_\\)+\\)")
          (requires . 1)
          (cache)))

      (let ((syntaxes '("use" "import" "select-module" "with-module" "extend" "define-in-module")))
        (ac-define-source gosh-modules
          `((candidates . gosh-ac-module-candidates)
            (symbol . "m")
            (prefix . ,(concat "(" (regexp-opt syntaxes) "[ \t\n]+\\(\\(?:\\sw\\|\\s_\\)+\\)"))
            (requires . 1)
            (cache))))

      (ac-define-source gosh-inferior-symbols
        '((candidates . gosh-ac-inferior-candidates)
          (symbol . "s")
          (requires . 2)
          (cache)))
    
      (add-to-list 'ac-modes 'gosh-mode)
      (add-to-list 'ac-modes 'gosh-inferior-mode))))

(defun gosh-ac-mode-initialize ()
  (when (featurep 'auto-complete)
    (add-to-list 'ac-sources 'ac-source-gosh-functions)
    (add-to-list 'ac-sources 'ac-source-gosh-symbols)
    (add-to-list 'ac-sources 'ac-source-gosh-keywords)
    (add-to-list 'ac-sources 'ac-source-gosh-modules)))

(defmacro gosh-map-env-symbols (env var &rest form)
  (declare (indent 2))
  `(mapc
    (lambda (env)
      (mapc
       (lambda (,var)
         (when (symbolp (car ,var))
           ,@form))
       env))
    ,env))

(defun gosh-ac-inferior-candidates ()
  (gosh-inferior-symbol-candidates))

;; keywords match to current context
(defun gosh-ac-keywords-candidates ()
  (let ((fnsym (gosh-parse-fnsym-current-sexp)))
    (when (and (cadr fnsym) (> (cadr fnsym) 0))
      (let ((fn (caar fnsym))
            (env (gosh-parse-current-env))
            (res '())
            key-args)
        (gosh-map-env-symbols env c
          (when (and (eq (car c) fn)
                     (consp (cadr c))
                     (memq (caadr c) '(lambda syntax)))
            (setq key-args (cdr (member :key (cadadr c))))
            (setq res (append 
                       (mapcar (lambda (k)
                                 (gosh-ac-symbol->keyword-string 
                                  (if (consp k)
                                      (car k)
                                    k)))
                               key-args)
                       res))))
        res))))

(defun gosh-ac-symbol->keyword-string (sym)
  (let ((name (symbol-name sym)))
    (cond
     ((keywordp sym)
      name)
     ((string-match "^#" name) "")
     (t
      (concat ":" name)))))

;; all symbols
(defun gosh-ac-symbol-candidates ()
  (let ((env (gosh-parse-current-env))
        (res '()))
    (gosh-map-env-symbols env inf
      (setq res (cons (symbol-name (car inf)) res)))
    res))

;; symbols that have lambda expression
(defun gosh-ac-function-candidates ()
  (let ((env (gosh-parse-current-env))
        (res '()))
    (gosh-map-env-symbols env inf
      (when (and (consp (cadr inf)) 
                 (eq (caadr inf) 'lambda))
        (setq res (cons (symbol-name (car inf)) res))))
    res))

(defun gosh-ac-module-candidates ()
  (gosh-available-modules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
;; indentation handling

(defvar calculate-lisp-indent-last-sexp)

;; Copied from scheme-indent-function, but ignore
;; scheme-indent-function property when be local variable.
(defun gosh-smart-indent (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (line-beginning-position 2)
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
             (method (and (not (assq function-sym (gosh-parse-context-local-vars)))
                          (get function-sym 'scheme-indent-function)))
             pair)
        (cond 
         ((or (eq method 'defun)
              (and (null method)
                   (> (length function) 3)
                   (string-match "\\`def" function)))
          (lisp-indent-defform state indent-point))
         ((integerp method)
          (lisp-indent-specform method state
                                indent-point normal-indent))
         ;;TODO
         ;; ((setq pair (gosh-assoc-to-regexp function))
         ;;  (lisp-indent-specform (cdr pair) state indent-point normal-indent))
         (method
          (funcall method state indent-point normal-indent)))))))

(defvar gosh-smart-indent-alist
  '(
    ))

;;TODO
(defun gosh-assoc-to-regexp (symbol)
  (let (maxlen max)
    (mapc
     (lambda (x) 
       (when (and (consp x)
                  (car x)
                  (numberp (cdr x)))
         (when (string-match (car x) symbol)
           (let ((len (- (match-end 0) (match-beginning 0))))
             ;;TODO match same length
             (when (or (null maxlen) (> len maxlen))
               (setq maxlen len)
               (setq max x))))))
     gosh-smart-indent-alist)
    max))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend

(defun gosh-backend-kill ()
  ;;TODO create ui
  (let ((proc (gosh-backend-active-process)))
    (when proc
      (kill-process proc))))

(defun gosh-backend-eval (sexp-string)
  ;;TODO unbaranced parenthese must be error before send.
  (let* ((hash (concat (md5 sexp-string) " ")) ;; hash string is separator.
         (proc (gosh-backend-check-process))
         (output-file (process-get proc 'gosh-backend-output-file))
         (eval-form 
          (format gosh-backend-eval-guard-format hash
                  (format
                   gosh-eval-expression-command-format 
                   output-file
                   sexp-string)))
         result)
    (setq result (gosh-backend-low-level-eval eval-form proc))
    (if (string-match (concat "^" hash "\\(.*\\)") result)
        (signal 'gosh-backend-error (list (match-string 1 result)))
      result)))

(defun gosh-backend-eval-get-output ()
  ;; call after executing `gosh-backend-eval'
  (let* ((proc (gosh-backend-check-process))
         (file (process-get proc 'gosh-backend-output-file)))
    ;; ignore huge file. not concern about error.
    (with-temp-buffer
      (let* ((cs (process-coding-system proc))
             (coding-system-for-read (car cs)))
        (insert-file-contents file)
        (buffer-string)))))

(defun gosh-backend-low-level-eval (sexp-string &optional process)
  (let* ((proc (or process (gosh-backend-check-process)))
         start end)
    (with-current-buffer (process-buffer proc)
      (gosh-backend-wait-locking proc)
      (process-put proc 'gosh-backend-locking t)
      (unwind-protect
          (progn
            (setq start (point-max))
            (process-send-string proc sexp-string)
            (let ((inhibit-quit t))
              ;; wait output from command
              (while (= start (point-max))
                (gosh-backend-wait proc))
              ;; wait return prompt from process.
              (while (not (gosh-backend-prompt-match))
                (gosh-backend-wait proc))
              ;;remove trailing newline
              (setq end (1- (match-beginning 0))))
            (unless end
              (signal 'quit nil)))
        (process-put proc 'gosh-backend-locking nil))
      (buffer-substring start end))))

(put 'gosh-backend-error 'error-conditions '(gosh-backend-error error))
(put 'gosh-backend-error 'error-message "Gosh error")

(defvar gosh-backend-suppress-discard-input nil)

;; Should use `sleep-for' not `sit-for'.
;; To indicate differences, compare following two examples.
;; In `sit-for' loop, type any key then `sit-for' means nothing.
;;
;; (while t
;;   (message "%s.%s" (format-time-string "%c") (nth 2 (current-time)))
;;   (sit-for 0.5))

;; (while t
;;   (message "%s.%s" (format-time-string "%c") (nth 2 (current-time)))
;;   (sleep-for 0.5))

(defun gosh-backend-wait-locking (proc)
  (while (process-get proc 'gosh-backend-locking)
    (unless gosh-backend-suppress-discard-input
      (discard-input))
    (sleep-for 0.1)))

(defun gosh-backend-wait (proc)
  (sleep-for 0.1)
  (unless gosh-backend-suppress-discard-input
    (discard-input))
  (when quit-flag
    (kill-process proc)
    (setq inhibit-quit nil)
    (signal 'quit nil)))

(defvar gosh-backend-prompt-string-regexp "\ngosh>[ \t]*$")
(defvar gosh-backend-prompt-regexp "^gosh>[ \t]*\\'")
(defconst gosh-backend-process-buffer-format " *Gosh-mode<%s>* ")
(defvar gosh-backend-process-alist nil)

(defun gosh-backend-prompt-match ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (looking-at gosh-backend-prompt-regexp)))

(defun gosh-backend-process-filter (proc event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert event)))

(defun gosh-backend-active-process (&optional command)
  (let* ((command (or command gosh-default-command-internal))
         proc)
    (cond
     ((and (setq proc (cdr (assoc command gosh-backend-process-alist)))
           (eq (process-status proc) 'run))
      proc)
     (proc
      (delete-process proc)
      nil)
     (t nil))))

(defun gosh-backend-check-process ()
  (unless gosh-default-command-internal
    (error "TODO"))
  ;;TODO switch by executable
  (let* ((command gosh-default-command-internal)
         (proc (gosh-backend-active-process command)))
    (unless proc
      (let* ((buffer (gosh-backend-process-buffer command)))
        (with-current-buffer buffer
          (setq proc (start-process "Gosh backend" buffer command "-i"))
          (set-process-filter proc 'gosh-backend-process-filter)
          (gosh-set-alist 'gosh-backend-process-alist command proc)
          ;; wait for first prompt
          (while (not (gosh-backend-prompt-match))
            (sleep-for 0.1)))))
    (let ((file (process-get proc 'gosh-backend-output-file)))
      (unless (and file (file-exists-p file))
        (process-put proc 'gosh-backend-output-file (make-temp-file "gosh-mode-output-"))))
    proc))

(defun gosh-backend-process-buffer (command)
  (let ((buffer (get-buffer-create (format gosh-backend-process-buffer-format command))))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (erase-buffer))
    buffer))

(defvar gosh-sticky-backend-temp-file nil)
(make-variable-buffer-local 'gosh-sticky-backend-temp-file)

;; TODO detect changing GAUCHE_LOAD_PATH, GAUCHE_DYNLOAD_PATH??
;;    but no way to remove from *load-path*
;; http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3aWishList&l=ja#H-1mx5tqr92ymmz
;; TODO detect changing environment-variable 
;;    sys-putenv
;;    evaluate sexp contains sys-putenv...
;;    no sys-unsetenv ...
(defun gosh-sticky-backend-switch-context ()
  (let* ((proc (gosh-backend-check-process))
         (file (gosh-sticky-backend-loading-file))
         (directory (expand-file-name default-directory)))
    (unless (file-directory-p directory)
      (error "Directory is not exist"))
    ;; filter is `gosh-sticky-switch-context-filter' means now switching context
    (when (and (not (eq (process-filter proc) 'gosh-sticky-switch-context-filter))
               (or (not (eq (process-get proc 'gosh-backend-current-buffer)
                            (current-buffer)))
                   (null (process-get proc 'gosh-backend-switched-time))
                   (null gosh-buffer-change-time)
                   (>= gosh-buffer-change-time 
                       (process-get proc 'gosh-backend-switched-time))))
      (set-process-filter proc 'gosh-sticky-switch-context-filter)
      (process-put proc 'gosh-backend-current-buffer (current-buffer))
      (process-put proc 'gosh-backend-switched-time (float-time))
      (process-put proc 'gosh-backend-locking t)
      (process-send-string proc (format "(sys-chdir \"%s\")\n" directory))
      proc)))

(defun gosh-sticky-backend-chdir ()
  (let* ((edir (expand-file-name default-directory))
         (gdir (funcall gosh-default-path-e2g edir)))
    (unless (file-directory-p edir)
      (error "Directory is not exist"))
    (gosh-backend-low-level-eval (format "(sys-chdir \"%s\")\n" gdir))))

(defun gosh-sticky-switch-context-filter (proc event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert event)
    (when (gosh-backend-prompt-match)
      ;; restore filter and unlock buffer to be enable evaluate expression
      (set-process-filter proc 'gosh-backend-process-filter)
      (process-put proc 'gosh-backend-locking nil))))

(defun gosh-sticky-backend-loading-file ()
  (let (file)
    (if (and buffer-file-name
             (not (buffer-modified-p (current-buffer)))
             ;; in archive file
             (file-exists-p buffer-file-name))
        (setq file buffer-file-name)
      (unless gosh-sticky-backend-temp-file
        (setq gosh-sticky-backend-temp-file 
              (make-temp-file "gosh-mode-")))
      (setq file gosh-sticky-backend-temp-file)
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region (point-min) (point-max) file nil 'no-msg)))
    file))

;;
;; sticky evaluation
;;

(defun gosh--temp-message (format-string &rest args)
  (let (message-log-max)
    (apply 'message format-string args)))

(defmacro gosh--processing-message (message &rest form)
  (declare (indent 1))
  `(progn
     (gosh--temp-message ,message)
     ,@form
     (gosh--temp-message (concat ,message "done"))))

(defun gosh-send-last-sexp ()
  "Send the previous sexp to the sticky backend process.
That sexp evaluated at current module"
  (interactive)
  (gosh-eval--check-backend)
  (if (gosh-parse-last-expression-define-p)
      (gosh-eval--last-toplevel-expression)
    (gosh-eval--send-region (save-excursion (backward-sexp) (point)) (point))))

(defvar gosh-read-expression-history nil)

(defun gosh-eval-defun ()
  "Evaluate current top level definition.
That sexp evaluated at current module"
  (interactive)
  (gosh-eval--check-backend)
  (save-excursion
    (end-of-defun)
    (gosh-eval--last-toplevel-expression)))

(defun gosh-eval-expression (eval-expression-arg)
  "Evaluate EVAL-EXPRESSION-ARG at current module.
And print value in the echo area.

"
  (interactive
   (list (let ((minibuffer-completing-symbol t))
           (read-from-minibuffer "Gosh Eval: "
                                 nil read-expression-map nil
                                 'gosh-read-expression-history))))
  (gosh-eval--check-backend)
  (gosh-eval-expression-1 eval-expression-arg))

(defun gosh-eval-buffer ()
  "Evaluate current buffer."
  (interactive)
  (gosh-eval--check-backend)
  (gosh--processing-message "Evaluating buffer..."
    (let ((file (gosh-sticky-backend-loading-file)))
      (gosh-backend-eval (format "(load \"%s\")\n" file)))))

(defun gosh-eval-region (start end)
  "Evaluate current region at current context."
  (interactive "r")
  (gosh-eval--check-backend)
  (gosh--processing-message "Evaluating region..."
    (let ((file (make-temp-file "gosh-mode-")))
      (unwind-protect
          (progn
            (let ((coding-system-for-write buffer-file-coding-system))
              (write-region start end file nil 'no-msg))
            (gosh-backend-eval (format "(load \"%s\")\n" file)))
        (delete-file file)))))

(defun gosh-eval-expression-1 (eval-expression-arg &optional suppress-message)
  (let ((module (gosh-parse-context-module))
        form)
    (setq form (format "(with-module %s %s)" 
                       module eval-expression-arg))
    (let (result output)
      (condition-case err
          (progn
            (setq result (gosh-backend-eval form))
            (setq output (gosh-backend-eval-get-output)))
        (gosh-backend-error
         (setq output (gosh-backend-eval-get-output))
         ;;emulate emacs error..
         (message "%s" output)
         (signal 'gosh-backend-error (cdr err))))
      (if suppress-message
          (message "%s" output)
        (message "%s%s" output result)))))

(defun gosh-eval--last-toplevel-expression ()
  (let* ((end (point))
         (module (gosh-parse-context-module))
         start sexp)
    (save-excursion
      (beginning-of-defun)
      (setq start (point))
      (setq sexp
            (format "(begin (select-module %s) %s)\n"
                    module
                    (buffer-substring start end)))
      (let ((result (gosh-backend-low-level-eval sexp)))
        ;; FIXME toplevel form error message probablly contains newline...
        (when (string-match "\n" result)
          (signal 'gosh-backend-error (list (format "%s" result))))
        (message "%s" result)))))

(defun gosh-eval--send-region (start end)
  (save-excursion
    ;; evaluate at end module context
    (goto-char end)
    (gosh-eval-expression (buffer-substring start end))))

(defun gosh-eval--check-backend ()
  (unless gosh-sticky-mode 
    (error "Command disabled when `gosh-sticky-mode' is disabled"))
  (if (gosh-backend-active-process)
      ;; backend already activated
      (gosh-sticky-backend-chdir)
    ;; backend deactivated then activate and load current file.
    ;; this case block several seconds in `gosh-backend-eval'
    (gosh-sticky-backend-switch-context)))

;;TODO
(defun gosh-test-module ()
  (interactive)
  (let ((mod (gosh-parse-context-module)))
    ;;TODO
    (gosh-sticky-validate-async mod)))

;; todo irresponsible function...
(defun gosh-popup-test-result ()
  (interactive)
  (cond
   ((null gosh-sticky-test-result)
    (message "No error."))
   (t
    (let ((msg (cadr (memq 'help-echo gosh-sticky-test-result))))
      (if msg
          (message "%s" msg)
        (message "No error."))))))



(defvar gosh-current-executable nil)
(make-variable-buffer-local 'gosh-current-executable)

(defun gosh-current-executable ()
  (or gosh-current-executable
      (setq gosh-current-executable
            (or (save-excursion
                  (goto-char (point-min))
                  (when (looking-at auto-mode-interpreter-regexp)
                    (let ((command (match-string-no-properties 2)))
                      command)))
                (and buffer-file-name
                     (gosh-guessed-executable 
                      (expand-file-name buffer-file-name)))
                gosh-default-command-internal))))

(defun gosh-guessed-executable (file)
  (catch 'found
    (mapc
     (lambda (item)
       (mapc
        (lambda (path)
          (when (string-match (concat "^" (regexp-quote path)) file)
            (throw 'found (nth 0 item))))
        (nth 3 item)))
     gosh-command-alist)
    gosh-default-command-internal))



(defvar gosh-buffer-change-time nil)
(make-variable-buffer-local 'gosh-buffer-change-time)

(defun gosh-after-change-function (start end old-len)
  ;;check executable
  (when (< end 255)
    (setq gosh-current-executable nil))
  (setq gosh-buffer-change-time (float-time)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refactor 

(defvar gosh-refactor-read-symbol-history nil)

(defun gosh-refactor-trim-expression (s)
  (if (string-match "^[ \t\n]*\\(\\(?:.\\|\n\\)*\\)" s)
      (match-string 1 s)
    s))

(defun gosh-refactor-read-sexp-string ()
  (gosh-refactor-trim-expression
   (buffer-substring (point)
                     (progn (forward-sexp 1) (point)))))

(defun gosh-refactor-goto-top-of-form ()
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
(defun gosh-refactor-find-binding-region (name)
  (catch 'done
    (let ((name-regexp (erefactor-create-regexp name)))
      (save-excursion
        (while (gosh-refactor-goto-top-of-form)
          (let ((start (point))
                (end (save-excursion (forward-sexp) (point)))
                function arg)
            ;; skip parenthese
            (forward-char)
            (setq function (gosh-refactor-read-sexp-string))
            (cond
             ((member function '("let" "let*" "letrec" "let1"
                                 "if-let1" "rlet" "and-let*" "fluid-let"
                                 "receive" "lambda"))
              ;;TODO named let
              ;; FIXME incorrect about let* and let
              (setq arg (gosh-refactor-read-sexp-string))
              (when (string-match name-regexp arg)
                (throw 'done (cons start end))))
             ((member function '("define"))
              (setq arg (gosh-refactor-read-sexp-string))
              (when (string-match name-regexp arg)
                ;;FIXME dirty
                (goto-char start)
                (gosh-refactor-goto-top-of-form)
                (let ((start (point))
                      (end (save-excursion (forward-sexp) (point))))
                  (unless (save-excursion 
                            (gosh-refactor-goto-top-of-form))
                    ;; Top of file
                    (setq end (point-max)))
                  (throw 'done (cons start end))))))
            (goto-char start)))))
    nil))

(defun gosh-refactor-rename-symbol-read-args ()
  (erefactor-rename-symbol-read-args 'gosh-refactor-read-symbol-history))

(defun gosh-refactor-find-executable-scripts ()
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
        (directory-files path t "^[^.]")))
     exec-path)
    list))

(defun gosh-refactor-rename-symbol (old-name new-name)
  "Rename symbol at point."
  (interactive (gosh-refactor-rename-symbol-read-args))
  (let (region)
    (setq region (gosh-refactor-find-binding-region old-name))
    (erefactor-rename-region old-name new-name region)))

;;TODO
;; *load-path* files
;; writable PATH files?
;; name -> dwim?
;; this symbol is not defined in this module.
;;  jump to the module and try again.
(defun gosh-refactor-rename-symbol-afaiui (old-name new-name)
  "Rename symbol As Far As I Understand It"
  ;; (split-string (getenv "GAUCHE_LOAD_PATH") path-separator)
  ;; (scheme-current-globals)
  )



(defun gosh-info-lookup-add-help (mode)
  (info-lookup-add-help
   ;; For
   ;;  info-complete-symbol (to complete a symbol using the info)
   ;;  info-lookup-symbol   (to look up a symbol in the info)
   :topic 'symbol
   :mode  mode
   :regexp "[^()'\" \t\n]+"
   :ignore-case nil
   :doc-spec gosh-info-appendixes
   :parse-rule  nil
   :other-modes nil))

(defun gosh-info-lookup-initialize ()
  (gosh-info-lookup-add-help 'gosh-mode)
  (gosh-info-lookup-add-help 'gosh-inferior-mode))



(provide 'gosh-mode)

;;; gosh-mode.el ends here
