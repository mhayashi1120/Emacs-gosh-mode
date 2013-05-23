;;; gosh-mode.el --- Programming language gauche editing tools.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: lisp gauche scheme edit
;; URL: https://github.com/mhayashi1120/Emacs-gosh-mode/raw/master/gosh-mode.el
;; Emacs: GNU Emacs 23 or later
;; Version: 0.3.0

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

;; * cmuscheme C-c C-t to trace. what is this?
;; * split backend to?
;;   each executable script.
;;   module script. (all module have one backend(?))
;; * create parser process that is separated from backend process?
;; * independent from flymake
;;
;; scheme-send-last-sexp to any keybind

;; gosh-eval-buffer to C-c C-b
;; gosh-eval-region to C-c C-n

;; * unload `user' module except followings
;;   *program-name*, *argv*

;; * regulate gosh-sticky-* gosh-eval-*

;; * Load automatically if module file is on the *load-path*?
;;   => Bad idea. Increase security risk

;; * gosh-show-info
;;   when :prefix symbol

;; * risky-local-variable

;; * re-consider find-file-noselect
;;   remove history? or use other low level api?

;; * auto bracket
;;   prefixed symbol

;;; Code:


(defgroup gosh-mode nil
  "Gauche script editing mode."
  :group 'lisp
  :prefix "gosh-")

(defvar gosh-mode-version "0.2.3")

(eval-when-compile
  (require 'cl)
  (require 'gosh-const))

(require 'gosh-const)
(require 'eldoc nil t)
(require 'scheme)
(require 'cmuscheme)
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


;;;
;;; utilities
;;;

;; [SRFI-2]
;; http://srfi.schemers.org/srfi-2/srfi-2.html
;;
;; AND-LET* (CLAWS) BODY

;; CLAWS ::= '() | (cons CLAW CLAWS)
;; CLAW  ::=  (VARIABLE EXPRESSION) | (EXPRESSION) | BOUND-VARIABLE
(defmacro gosh-and* (varlist &rest body)
  (declare (indent 1) (debug t))
  (reduce
   (lambda (v res)
     (cond
      ((atom v)
       ;; BOUND-VARIABLE
       `(and ,v ,res))
      ((= (length v) 1)
       ;; (EXPRESSION)
       `(and ,@v ,res))
      ((> (length v) 2)
       (error "Malformed `and-let*'"))
      ((not (symbolp (car v)))
       (error "Malformed `and-let*'"))
      (t
       ;; (VARIABLE EXPRESSION)
       `(let ((,(car v) ,(cadr v)))
          (and ,(car v) ,res)))))
   varlist
   :from-end t
   :initial-value `(progn ,@body)))

(defmacro gosh-if-let1 (var expr then &rest else)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2))
  `(let ((,var ,expr))
     (if ,var ,then ,@else)))

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

(defun gosh--scan-sexps (point count)
  (condition-case nil
      (scan-sexps point count)
    (scan-error nil)))

(defun gosh-symbol-eq (symbol1 symbol2)
  (let ((name1 (symbol-name symbol1))
        (name2 (symbol-name symbol2)))
    (eq (gosh-intern name1) (gosh-intern name2))))

(defun gosh-symbol-p (symbol)
  (and symbol
       ;;TODO nil in scheme symbol
       (symbolp symbol)))

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

(defun gosh-paren-against-char (char)
  (case char
    (?\( ?\))
    (?\[ ?\])
    (?\) ?\()
    (?\] ?\[)))

;;
;; alist, list utilities
;;

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

(defun gosh-env-filter (pred env)
  (mapcar 'car
          (apply 'concatenate
                 'list
                 (mapcar (lambda (e) (gosh-filter pred e)) env))))

(defun gosh-filter (pred list)
  (loop for l in list
        if (funcall pred l)
        collect l))

(defun gosh-remove (pred list)
  (loop for l in list
        unless (funcall pred l)
        collect l))

(defun gosh-find (pred list)
  (loop for l in list
        if (funcall pred l)
        return l))

(defun gosh-intersection (list1 list2)
  (loop for l in list1
        if (member l list2)
        collect l))

(defun gosh-union (list1 list2)
  (loop with res = list1
        for l in list2
        unless (member l res)
        do (setq res (cons l res))
        finally return res))

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


;;
;; string utilities
;;

(defun gosh-string-split-word (string)
  (loop with start = 0
        while (string-match "\\w+" string start)
        collect (progn
                  (setq start (match-end 0))
                  (match-string 0 string))))

(defun gosh-string-starts-with (pref str)
  (let ((p-len (length pref))
        (s-len (length str)))
    (and (<= p-len s-len)
         (equal pref (substring str 0 p-len)))))


;;
;; file utilities
;;

(defun gosh-any-file-in-path (file path)
  (car (gosh-filter
        (lambda (dir) (file-exists-p (concat dir "/" file)))
        path)))

(defun gosh-directory-tree-files (init-dir &optional match)
  (when (file-directory-p init-dir)
    (let ((res '())
          (stack (list init-dir)))
      (while (consp stack)
        (let* ((dir (pop stack))
               (files (cddr (directory-files dir))))
          (dolist (file files)
            (let ((filename (expand-file-name file dir)))
              (cond
               ((string-match "^\\." file))
               ((file-directory-p filename)
                (push filename stack))
               ((and match (string-match match file))
                (setq res (cons filename res))))))))
      res)))

(defun gosh--find-file-noselect (file &optional nowarn)
  (let* ((file-name-history)
         (buf (find-file-noselect file nowarn)))
    buf))

(defmacro gosh-with-find-file (path-expr &rest body)
  (declare (indent 1))
  (let ((path (make-symbol "path"))
        (buf (make-symbol "buf"))
        (res (make-symbol "res")))
    `(let* ((,path (file-truename ,path-expr))
            (,buf (get-file-buffer ,path)))
       (with-current-buffer (or ,buf (gosh--find-file-noselect ,path t))
         (let (,res)
           (unwind-protect
               (setq ,res (ignore-errors (save-excursion ,@body)))
             (unless ,buf
               (kill-buffer (current-buffer))))
           ,res)))))

(defun gosh-file-mtime (file)
  (nth 5 (file-attributes file)))


;;;
;;; Gauche sexp <-> Emacs sexp
;;;

;; list -> list
;; number -> number or gosh-object[number]
;; vector -> gosh-object[vector] 
;; string -> string
;; interporate-string -> string
;; regexp -> gosh-object[regexp]

(defun gosh-object-p (obj)
  (and (vectorp obj) (= (length obj) 2)))

(defun gosh-object-type (obj)
  (aref obj 0))

(defun gosh-object-value (obj)
  (aref obj 1))

(defun gosh-object (type value)
  (case type
    (char (vector 'char value))
    (charset (vector 'charset value))
    (number (vector 'number value))
    (vector (vector 'vector value))
    (regexp (vector 'regexp value))
    ((u8vector u16vector u32vector u64vector
               s8vector s16vector s32vector s64vector
               f16vector f32vector f64vector)
     (vector type value))
    (unquote (vector 'unquote value))
    (uninterned-symbol (vector 'uninterned-symbol value))
    (reader-constructor (vector 'reader-constructor value))
    (back-reference (vector 'back-reference value))
    (t (error "Not a supported type %s" type))))

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


;;;
;;; Gauche pseudo reader
;;;

;; TODO reconsider all parsing use this
(defconst gosh-reader--ws "\t\n\x0b\f\r\s\x3b\xa0")

;; read.c ctypes[]
(defconst gosh-reader--word-re
  (let ((chars "][\000-\037\s\"'(),;\\`{|}\177"))
    (format "\\(\\(?:\\\\.\\|[^%s]\\)+\\)"
            chars)))

(defconst gosh-reader--string-re "\"\\(\\(?:\\\\\.\\|[^\"]\\)*\\)\"")

;; (with-input-from-string "#!fold-case ABCD\n ABCD"
;;   (^() (read-line) (read)))
;; => ABCD
(defvar gosh-reader--fold-case nil)
(make-variable-buffer-local 'gosh-reader--fold-case)

(defun gosh-reader--maybe-number (text)
  (let ((n (string-to-number text)))
    (cond
     ;; only accept as emacs number if transeposable text.
     ((equal (number-to-string n) text)
      n)
     ;; FIXME:
     ;; only handle simple number format.
     ((string-match "\\`[-+]?[0-9]+\\(\\.[0-9]+\\)?\\'" text)
      (gosh-object 'number text))
     (t nil))))

(defun gosh-reader--word-to-datum (text)
  (let ((n (gosh-reader--maybe-number text)))
    (cond
     (n n)
     ((not gosh-reader--fold-case)
      (intern text))
     (t
      (intern (downcase text))))))

(defun gosh-reader-ignore ()
  (let ((start (point)))
    (while (or (plusp (gosh-reader--skip-ws))
               (plusp (gosh-reader--skip-comment))
               (plusp (gosh-reader--skip-debug-macro))
               (plusp (gosh-reader--skip-meta))))
    (- (point) start)))

(defun gosh-reader--skip-meta ()
  (cond
   ((looking-at "#![\s/]")
    (gosh-reader--skip-to-match ".*\n"))
   ((looking-at "#!")
    (let ((start (point)))
      (forward-char 2)
      (let ((word (gosh-reader--read)))
        (cond
         ((eq word 'fold-case)
          (setq gosh-reader--fold-case t))
         ((eq word 'no-fold-case)
          (setq gosh-reader--fold-case nil))
         (t
          ;; gauche reader show the message
          ;; but this gosh-reader do not.
          ))
        (- (point) start))))
   (t
    0)))

(defun gosh-reader--skip-debug-macro ()
  (cond
   ((looking-at "#\\?")
    (goto-char (match-end 0))
    (+ 2 (gosh-reader--skip-word)))
   (t 0)))

(defun gosh-reader--skip-ws ()
  (skip-chars-forward gosh-reader--ws))

(defun gosh-reader--skip-nested-comments ()
  ;; already read first `#|'
  (let ((nested 1)
        (start (point)))
    (while (> nested 0)
      (cond
       ((eobp)
        (signal 'invalid-read-syntax '("todo")))
       ((looking-at "#|")
        (setq nested (1+ nested))
        (forward-char 2))
       ((looking-at "|#")
        (setq nested (1- nested))
        (forward-char 2))
       (t
        (forward-char))))
    (- (point) start)))

(defun gosh-reader--skip-comment ()
  (cond
   ((eq (char-after) ?\;)
    (skip-chars-forward "^\n"))
   ((looking-at "#|")
    (goto-char (match-end 0))
    (gosh-reader--skip-nested-comments))
   ((looking-at "#;")
    (let ((start (point)))
      (goto-char (match-end 0))
      (gosh-reader--read)
      (- (point) start)))
   (t
    0)))

(defun gosh-reader--skip-word ()
  (let ((start (point)))
    (when (looking-at gosh-reader--word-re)
      (goto-char (match-end 0)))
    (- (point) start)))

(defun gosh-reader--skip-to-match (regexp)
  (unless (looking-at regexp)
    (signal 'invalid-read-syntax (list "TODO nothing to consume")))
  (goto-char (match-end 0))
  (length (match-string 0)))

(defun gosh-reader--read-by-regexp (regexp subexp)
  (unless (looking-at regexp)
    (signal 'invalid-read-syntax (list "todo")))
  (goto-char (match-end 0))
  (match-string-no-properties subexp))

(defun gosh-reader--read-bulk-literal (ending-char)
  (let ((start (point))
        (regexp (format "\\(\\(?:\\\\.\\|[^%c]\\)+%c\\)"
                        ending-char ending-char)))
    (forward-char)
    (unless (looking-at regexp)
      (signal 'invalid-read-syntax (list "todo msg")))
    (goto-char (match-end 0))
    (buffer-substring-no-properties start (point))))

(defun gosh-reader--read-bulk-string ()
  (forward-char)
  (unless (looking-at gosh-reader--string-re)
    ;;TODO msg
    (signal 'invalid-read-syntax nil))
  (goto-char (match-end 0))
  (match-string-no-properties 1))

(defun gosh-reader--sharp-vector ()
  (gosh-object 'vector (vconcat (gosh-reader--read-list ?\)))))

(defconst gosh-reader--sharp-char-symbol-alist
  '(
    ("space"   . ?\s  )
    ("newline" . ?\n  ) ("nl"      . ?\n  ) ("lf"      . ?\n  )
    ("return"  . ?\r  ) ("cr"      . ?\r  )
    ("tab"     . ?\t  ) ("ht"      . ?\t  )
    ("page"    . ?\f  )
    ("escape"  . ?\x1b) ("esc"     . ?\x1b)
    ("delete"  . ?\x7f) ("del"     . ?\x7f)
    ("null"    . ?\x00)
    ))

(defun gosh-reader--sharp-char ()
  (unless (looking-at gosh-reader--word-re)
    (signal 'invalid-read-syntax nil))
  (goto-char (match-end 1))
  (let ((text (substring (match-string-no-properties 1) 1))
        tmp)
    (gosh-object 'char
                 (cond
                  ((= (length text) 1)
                   (string-to-char text))
                  ((setq tmp (assoc text gosh-reader--sharp-char-symbol-alist))
                   (cdr tmp))
                  ((string-match "\\`[xX]\\([0-9a-fA-F]+\\)\\'" text)
                   (string-to-number (match-string 1 text) 16))
                  ((string-match "\\`[uU]\\([0-9a-fA-F]+\\)\\'" text)
                   (string-to-number (match-string 1 text) 16))
                  (t
                   (signal 'invalid-read-syntax (list text)))))))

(defun gosh-reader--sharp-uniform-vector ()
  ;; f16 is missing but have been checked at caller
  (unless (looking-at "\\([usUSfF]\\(?:8\\|16\\|32\\|64\\)\\)(")
    (signal 'invalid-read-syntax nil))
  (goto-char (match-end 1))
  (let* ((vsize (downcase (match-string-no-properties 1)))
         (vtype (intern (concat vsize "vector")))
         (list (gosh-reader--read-list ?\))))
    (gosh-object vtype (vconcat list))))

(defun gosh-reader--sharp-number (directive)
  (forward-char)
  (let ((start (point)))
    (unless (looking-at gosh-reader--word-re)
      (signal 'invalid-read-syntax nil))
    (goto-char (match-end 0))
    (gosh-object 'number
                 (format "#%c%s"
                         directive
                         (buffer-substring-no-properties start (point))))))

(defun gosh-reader--sharp-boolean (char)
  (forward-char)
  (cond
   ((eq char ?t)
    '\#t)
   ((eq char ?f)
    '\#f)
   (t (signal 'invalid-read-syntax `("Assert #" char)))))

(defun gosh-reader--sharp-regexp ()
  (gosh-object 'regexp (gosh-reader--read-bulk-literal ?\/)))

(defun gosh-reader--sharp-reference ()
  (cond
   ((looking-at "[0-9]+=")
    (goto-char (match-end 0))
    (gosh-reader--read))
   ((looking-at "\\([0-9]+\\)#")
    (goto-char (match-end 0))
    (let* ((text (match-string-no-properties 1))
           (num (string-to-number text)))
      (gosh-object 'back-reference num)))
   (t
    (signal 'invalid-read-syntax
            (list "invalid reference form")))))

(defun gosh-reader--sharp-constructor ()
  (forward-char)
  (let ((sexp (gosh-reader--read)))
    (gosh-object 'reader-constructor sexp)))

(defun gosh-reader--sharp-unintern-symbol ()
  (forward-char)
  (unless (looking-at gosh-reader--word-re)
    (signal 'invalid-read-syntax `("#:")))
  (gosh-object 'uninterned-symbol (match-string-no-properties 1)))

(defun gosh-reader--sharp-charset ()
  (let* ((regexp "\\[\\(\\(?:\\[[^\[]+?\\]\\|\\\\.\\|[^\]]\\)*?\\)\\]")
         (data (gosh-reader--read-by-regexp regexp 1)))
    (gosh-object 'charset data)))

;; TODO more sophisticate
(defun gosh-reader--read-list (end &optional limit)
  (forward-char)
  (let ((res '())
        (dot-ctx))
    (catch 'done
      (while t
        (gosh-reader-ignore)
        (when (eq (char-after) end)
          (forward-char)
          (cond
           ((not dot-ctx)
            (throw 'done (nreverse res)))
           ((eq dot-ctx 'finish)
            (throw 'done res))))
        (when (and limit (>= (length res) limit))
          (throw 'done (nreverse res)))
        (when (eq dot-ctx 'finish)
          (signal 'invalid-read-syntax (list "Invaild dot syntax")))
        (let ((item (gosh-reader--read)))
          (cond
           ((eq '\. item)
            (when (or dot-ctx (null res))
              (signal 'invalid-read-syntax (list "Invalid dot syntax")))
            (setq dot-ctx 'start))
           ((eq dot-ctx 'start)
            (setq res (append
                       (nreverse (cdr res))
                       (cons (car res) item)))
            (setq dot-ctx 'finish))
           (t
            (setq res (cons item res)))))))))

(defun gosh-reader--read-sharp-syntax ()
  (forward-char)
  (when (eobp)
    (signal 'end-of-file nil))
  (let ((begin (downcase (char-after))))
    (cond
     ((eq begin ?!)
      ;; this context should have already been skipped.
      (signal 'invalid-read-syntax `("Assert #!")))
     ((eq begin ?\()
      ;; vector
      (gosh-reader--sharp-vector))
     ((eq begin ?*)
      ;; If followed by a double quote, denotes an incomplete string.
      (gosh-reader--read-bulk-string))
     ((eq begin ?,)
      ;; [SRFI-10] Introduces reader constructor syntax.
      (gosh-reader--sharp-constructor))
     ((eq begin ?\/)
      ;; regexp
      (gosh-reader--sharp-regexp))
     ((memq begin '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
      ;; [SRFI-38] Shared substructure definition and reference.
      (gosh-reader--sharp-reference))
     ((eq begin ?:)
      ;; Uninterned symbol.
      (gosh-reader--sharp-unintern-symbol))
     ((eq begin ?\;)
      ;; [SRFI-62] S-expression comment. Reads begin one S-expression and discard it.
      ;; comment context should have already been skipped.
      (signal 'invalid-read-syntax `("Assert #;")))
     ((eq begin ?<)
      ;; Introduces an unreadable object.
      (signal 'invalid-read-syntax `("Unreadable object")))
     ((eq begin ?\?)
      ;; Introduces debug macros.
      (signal 'invalid-read-syntax `("Debug macro")))
     ((memq begin '(?b ?d ?e ?i ?o ?x))
      ;; [R5RS] Binary number prefix.
      ;; [R5RS] Decimal number prefix.
      ;; [R5RS] Exact number prefix.
      ;; [R5RS] Inexact number prefix.
      ;; [R5RS] Octal number prefix.
      ;; [R5RS] Hexadecimal number prefix.
      (gosh-reader--sharp-number begin))
     ((eq begin ?t)
      (gosh-reader--sharp-boolean ?t))
     ((looking-at "[fF]\\(16\\|32\\|64\\)")
      (gosh-reader--sharp-uniform-vector))
     ((eq begin ?f)
      (gosh-reader--sharp-boolean ?f))
     ((memq begin '(?u ?s))
      ;; [SRFI-4] introducing SRFI-4 uniform vector.
      ;; [SRFI-4] introducing SRFI-4 uniform vector.
      (gosh-reader--sharp-uniform-vector))
     ((eq begin ?\[)
      ;; Introduces a literal character set.
      (gosh-reader--sharp-charset))
     ((eq begin ?\\)
      ;; Introduces a literal character.
      (gosh-reader--sharp-char))
     ((eq begin ?\`)
      ;;Introduces an interpolated string.
      (gosh-reader--read-bulk-string))
     ((eq begin ?\|)
      ;; [SRFI-30] Introduces a block comment.
      ;; comment context should have already been skipped.
      (signal 'invalid-read-syntax `("Assert #|")))
     (t
      (signal 'invalid-read-syntax `(,(format "#%c" begin)))))))

(defun gosh-reader--read-escaped-symbol ()
  (forward-char)
  (unless (looking-at "\\(\\(?:\\\\.\\|[^|]\\)*\\)|")
    (signal 'invalid-read-syntax (list "Not a escaped symbol")))
  (goto-char (match-end 0))
  (intern (match-string-no-properties 1)))

(defun gosh-reader--read-unquote ()
  (forward-char)
  (when (eobp)
    (signal 'end-of-file nil))
  (gosh-object 'unquote (gosh-reader--read)))

(defun gosh-reader--read-quote ()
  (forward-char)
  (list 'quote (gosh-reader--read)))

(defun gosh-reader--read-string ()
  (unless (looking-at gosh-reader--string-re)
    (signal 'invalid-read-syntax (list "Non terminated string")))
  (goto-char (match-end 0))
  (match-string-no-properties 1))

(defun gosh-reader--read-datum ()
  (goto-char (match-end 0))
  (gosh-reader--word-to-datum (match-string-no-properties 1)))

;; TODO FIXME: reconsider 
(defun gosh-reader--read ()
  (gosh-reader-ignore)
  (when (eobp)
    (signal 'end-of-file nil))
  (let ((next (char-after)))
    (cond
     ((eq next ?\()
      (gosh-reader--read-list ?\)))
     ((eq next ?\[)
      (gosh-reader--read-list ?\]))
     ((eq next ?#)
      (gosh-reader--read-sharp-syntax))
     ((eq next ?\|)
      (gosh-reader--read-escaped-symbol))
     ((eq next ?,)
      (gosh-reader--read-unquote))
     ((memq next '(?\' ?\`))            ; ignore differences ' and `
      (gosh-reader--read-quote))
     ((eq next ?\")
      (gosh-reader--read-string))
     ((looking-at gosh-reader--word-re)
      (gosh-reader--read-datum))
     (t
      (signal 'invalid-read-syntax nil)))))

(defun gosh-read ()
  (gosh-reader--read))

;;TODO read-partial-list?
(defun gosh-read-list (limit)
  (gosh-reader-ignore)
  (let ((next (char-after)))
    (cond
     ((eq next ?\()
      (gosh-reader--read-list ?\) limit))
     ((eq next ?\[)
      (gosh-reader--read-list ?\] limit))
     (t
      (signal 'invalid-read-syntax nil)))))

(defun gosh-read-just-first (string)
  (car (gosh-read-from-string string)))

(defun gosh-read-from-string (string)
  (let ((result (gosh-read-context-string string)))
    (cons (nth 0 result) (nth 1 result))))

(defun gosh-read-context-string (string &optional context)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (dolist (x context)
      (set (car x) (cdr x)))
    (list (gosh-read) (1- (point))
          (delq nil (mapcar 
                     (lambda (x)
                       (and (string-match "\\`gosh-" (symbol-name (car-safe x)))
                            x))
                     (buffer-local-variables))))))

;;;
;;; Basic settings
;;;

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
   "(map (lambda (mod) (map (lambda (sym) "
   " (string-copy (symbol->string sym))) (module-exports mod))) "
   " (module-imports (current-module))))\n"))

(defvar gosh-autoload-modules
  '(null user gauche scheme))

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
;; (defvar gosh-delegate-command)
;; (defun gosh-delegate-command-get (index)
;;   ;; When debugging gosh-mode, execute function (ex: `eval-expression')
;;   ;; make unbound variable error.
;;   (let ((command (if (and gosh-debug
;;                           (not (boundp 'gosh-delegate-command)))
;;                      (gosh-current-executable)
;;                    gosh-delegate-command)))
;;     (nth index (assoc command gosh-command-alist))))

;; (defun gosh-delegate-version ()
;;   (gosh-delegate-command-get 1))
;; (defun gosh-delegate-repo-path ()
;;   (gosh-delegate-command-get 2))
;; (defun gosh-delegate-load-path ()
;;   (gosh-delegate-command-get 3))
;; (defun gosh-delegate-command-type ()
;;   (gosh-delegate-command-get 4))
;; (defun gosh-delegate-path-separator ()
;;   (gosh-delegate-command-get 5))
;; (defun gosh-delegate-path-g2e ()
;;   (gosh-delegate-command-get 6))
;; (defun gosh-delegate-path-e2g ()
;;   (gosh-delegate-command-get 7))

(defun gosh-register-command (command)
  (let* ((full (gosh--check-command command)))
    (unless full
      (error "Unable recognize as gosh command"))
    (or (assoc full gosh-command-alist)
        (let* ((output (gosh--call-command->string full full "-V"))
               (ver (when (string-match "version[\s\t]+\\([0-9][0-9.]+\\)" output)
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
                   (format "Command %s -> " gosh-default-command-internal)
                   gosh-command-alist nil nil
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
  (gosh-info-initialize))

(defun gosh--call-command->string (gosh command &rest args)
  (let ((dir (file-name-directory gosh)))
    (with-temp-buffer
      (apply 'call-process (expand-file-name command dir) nil (current-buffer) nil args)
      (buffer-string))))

(defun gosh--check-command (command)
  (let ((full (executable-find command)))
    (when (and full (string-match "/gosh\\(\\.exe\\)?$" full))
      full)))

(defun gosh-available-modules (&optional full)
  "All module symbols in *load-path*"
  (let ((paths (gosh-load-path)))
    (gosh-append-map
     (lambda (dir)
       (let ((len (length dir)))
         (mapcar
          (lambda (f)
            (if full
                f
              (subst-char-in-string
               ?/ ?. (file-name-sans-extension (substring f (+ 1 len))))))
          (gosh-directory-tree-files dir "\\.scm$"))))
     (mapcar 'directory-file-name paths))))

(defun gosh--module->file (mod)
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

(defun gosh-guessed-module-files (imported symbol)
  (let ((symnm (or (and (stringp symbol) symbol)
                   (symbol-name symbol)))
        (imports imported)
        1st 2nd 3rd)
    (let ((prefix (if (string-match "\\`\\([^-]+\\)" symnm)
                      (match-string 1 symnm)
                    symnm))
          (words (split-string symnm "\\b" t)))
      (dolist (mname (gosh-available-modules))
        (let* ((name (intern mname))
               (names (split-string mname "[.]"))
               (file (gosh--module->file name))
               (import-as (assq name imports)))
          (cond
           ((and import-as
                 (not (string= (nth 1 import-as) "")))
            (when (gosh-string-starts-with (nth 1 import-as) symnm)
              (push (cons file (nth 1 import-as)) 1st))
            (setq imports (delq import-as imports)))
           ((member prefix names)
            (push file 2nd))
           ((gosh-intersection words names)
            (push file 2nd))))))
    (dolist (m imports)
      (push (gosh--module->file (car m)) 3rd))
    (delq nil (append 1st 2nd 3rd))))

(defun gosh-current-executable ()
  (or (gosh-mode-get :executable)
      (let ((command
             (or (save-excursion
                   (goto-char (point-min))
                   (when (looking-at auto-mode-interpreter-regexp)
                     (let ((command (match-string-no-properties 2)))
                       command)))
                 (and buffer-file-name
                      (gosh-guessed-executable
                       (expand-file-name buffer-file-name)))
                 gosh-default-command-internal)))
        (gosh-mode-put :executable command))))

(defun gosh-guessed-executable (file)
  (catch 'found
    (dolist (item gosh-command-alist)
      (dolist (path (nth 3 item))
        (when (string-match (concat "^" (regexp-quote path)) file)
          (throw 'found (nth 0 item)))))
    ;; Not found default value
    gosh-default-command-internal))

;;
;; For cygwin path
;;

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
     ((string-match
       (format "^\\(?:%s\\)\\([a-zA-Z]\\)/\\(.*\\)"
               (regexp-quote cygdrive)) path)
      (format "%s:/%s" (match-string 1 path) (match-string 2 path)))
     ((string-match "^/" path)
      (expand-file-name (substring path 1) installed))
     (t
      path))))

(defun gosh-emacs-path->cygpath (path)
  (let ((abspath (expand-file-name path))
        (cygdrive gosh-cygwin-cygdrive)
        (installed gosh-cygwin-directory)
        (case-fold-search t))
    (cond
     ((not (file-name-absolute-p path))
      path)
     ((string-match (concat "^" (regexp-quote installed) "\\(.*\\)") abspath)
      (concat "/" (match-string 1 abspath)))
     ((string-match "^\\([a-zA-Z]\\):/\\(.*\\)" abspath)
      (format "%s%s/%s" cygdrive (match-string 1 abspath) (match-string 2 abspath)))
     ((string-match "^/" abspath)
      (expand-file-name (substring abspath 1) installed))
     (t
      path))))


;;;
;;; Parse buffer
;;;

(defun gosh-context-string-p (&optional point)
  (save-excursion
    (let ((context (parse-partial-sexp (point-min) (or point (point)))))
      (cond
       ((and context (nth 3 context))
        ;; handling gauche extend definition
        (let ((beg (nth 8 context)))
          (goto-char beg)
          (when (looking-back "#`?")
            (setq beg (match-beginning 0)))
          (cons (nth 3 context) beg)))
       ((looking-back "#`?")
        (cons ?\" (match-beginning 0)))))))

(defun gosh-context-comment-p (&optional point)
  (save-excursion
    (let ((context (parse-partial-sexp (point-min) (or point (point)))))
      (when (and context (nth 4 context))
        (cons (nth 4 context) (nth 8 context))))))

(defun gosh-context-code-p (&optional point)
  (save-excursion
    (let ((context (parse-partial-sexp (point-min) (or point (point)))))
      (and (not (nth 3 context))
           (not (nth 4 context))))))

(defun gosh-context-toplevel-p (&optional point)
  (not (nth 9 (parse-partial-sexp (point-min) (or point (point))))))

;; returns current argument position within sexp
(defun gosh-beginning-of-current-sexp-operator ()
  (let ((pos 0))
    (skip-syntax-backward "w_")
    (while (and (not (bobp)) (not (eq ?\( (char-syntax (char-before)))))
      (gosh-parse--upward-sexp)
      (incf pos))
    pos))

;; goto beginning of current sexp (_ is cursor)
;; See `gosh-mode-test--BoL' at gosh-mode-test.el
;;TODO cleanup
(defun gosh-beginning-of-list ()
  (let ((first (point))
        top)
    (gosh-beginning-of-string)
    (catch 'done
      (while t
        (condition-case err
            (backward-sexp)
          (scan-error
           (skip-chars-backward gosh-reader--ws)
           (when (eq ?\( (char-syntax (char-before)))
             (setq top nil)
             (throw 'done t))))
        (when (and (eq ?\( (char-syntax (char-after)))
                   (not (bobp)))
          (backward-char))
        (when (or (setq top (eq ?\( (char-syntax (char-after))))
                  (bobp))
          (throw 'done t))))
    (unless (or (bobp) top)
      (backward-char))
    (not (eq first (point)))))

(defun gosh-beginning-of-string ()
  (let ((context (gosh-context-string-p)))
    (when context
      (goto-char (cdr context)))))

(defun gosh-end-of-string ()
  (let ((context (gosh-context-string-p)))
    (when context
      (goto-char (cdr context))
      (let ((end (gosh--scan-sexps (point) 1)))
        (when end
          (goto-char end))))))

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
        (gosh-read-just-first (buffer-substring (point) end))))))

;;;
;;; parsing program which may contain unbalanced parenthese/bracket
;;;

;; move upward as long as can.
(defun gosh-parse--upward-sexp ()
  (unless (bobp)
    (or (gosh-beginning-of-string)
        (let* ((char (char-before))
               (syn (char-syntax char)))
          (cond
           ((or (eq syn ?\() (memq char '(?\#)))
            (forward-char -1))
           (t
            (forward-sexp -1)))))))

;; goto next beginning of sexp. with ignoring context (e.g. #?= other comment)
(defun gosh-parse--next-sexp ()
  ;; move to end of string if is in string context
  (gosh-end-of-string)
  (unless (plusp (gosh-reader-ignore))
    (condition-case nil
        (forward-sexp)
      (scan-error
       (forward-char)))
    (gosh-reader-ignore)))

(defun gosh-parse-symbol-at-point ()
  (save-excursion
    (skip-syntax-backward "w_")
    (let ((start (point)))
      (skip-syntax-forward "w_")
      (and (< start (point))
           (intern (buffer-substring start (point)))))))

(defun gosh-parse-last-expression-define-p ()
  "Return t if last expression is top-level define-*"
  (and (gosh-context-toplevel-p)
       (save-excursion
         (beginning-of-defun)
         (looking-at "([\s\t\n]*define"))))

(defun gosh-parse--sexp-type-at-point (&optional env)
  (case (char-syntax (char-after))
    ;;TODO why check only list?
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
    (t
     (gosh-read))))

(defun gosh-parse--let-vars-at-point (&optional env limit loopp)
  (let ((end (min (or limit (point-max))
                  (or (ignore-errors
                        (save-excursion (forward-sexp) (point)))
                      (point-min))))
        (vars '()))
    (forward-char 1)
    (while (< (point) end)
      (when (eq (char-syntax (char-after)) ?\()
        (save-excursion
          (forward-char 1)
          (when (and loopp (looking-at "\\(for\\|let\\|with\\)\\>"))
            (gosh-parse--next-sexp))
          (when (eq ?w (char-syntax (char-after)))
            (let* ((sym (gosh-parse-symbol-at-point))
                   (type (and (not loopp)
                              (ignore-errors
                                (gosh-parse--next-sexp)
                                (gosh-parse--sexp-type-at-point env)))))
              (push (if type (list sym type) (list sym)) vars)))))
      (unless (ignore-errors
                (let ((here (point)))
                  (gosh-parse--next-sexp)
                  (> (point) here)))
        (goto-char end)))
    (reverse vars)))

(defun gosh-parse--extract-match-clause-vars (x)
  (cond
   ((null x) '())
   ((symbolp x)
    (if (memq x '(_ ___ \.\.\.))
        '()
      (list (list x))))
   ((consp x)
    (case (car x)
      ((or not)
       (gosh-parse--extract-match-clause-vars (cdr x)))
      ((and)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (consp (caddr x))
                (not (memq (caaddr x)
                           '(= $ @ ? and or not quote quasiquote get! set!))))
           (cons (list (cadr x) (if (listp (caddr x)) 'list 'pair))
                 (gosh-parse--extract-match-clause-vars (cddr x)))
         (gosh-parse--extract-match-clause-vars (cddr x))))
      ((= $ @)
       (if (consp (cdr x)) (gosh-parse--extract-match-clause-vars (cddr x)) '()))
      ((\? ? ) ; XXXX this is a hack, the lone ? gets read as a char (space)
       (if (and (consp (cdr x))
                (consp (cddr x))
                (symbolp (cadr x))
                (symbolp (caddr x)))
           (cons (list (caddr x) (gosh-predicate->type (cadr x)))
                 (gosh-parse--extract-match-clause-vars (cdddr x)))
         (gosh-parse--extract-match-clause-vars (cddr x))))
      ((get! set!)
       (if (consp (cdr x)) (gosh-parse--extract-match-clause-vars (cadr x)) '()))
      ((quote) '())
      ((quasiquote) '())                ; XXXX
      (t
       (gosh-union
        (gosh-parse--extract-match-clause-vars (car x))
        (gosh-parse--extract-match-clause-vars (cdr x))))))
   ((vectorp x)
    (gosh-parse--extract-match-clause-vars (concatenate 'list x)))
   (t
    '())))

;; call this from the first opening paren of the match clauses
(defun gosh-parse--extract-match-vars (&optional pos limit)
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
                               (gosh-parse--extract-match-clause-vars pat))))
              (setq match-vars (append new-vars match-vars)))
            (goto-char (if (or pos (not end)) limit (+ end 1)))))))
      match-vars)))

(defun gosh-parse--current-local-vars (&optional env)
  (let ((vars '())
        (start (point))
        (limit (save-excursion (beginning-of-defun) (+ (point) 1)))
        (let-limit (save-excursion
                     (ignore-errors
                       (gosh-parse--upward-sexp)
                       (gosh-parse--upward-sexp))
                     (point)))
        ;;TODO huh? not incremented.
        (scan-internal))
    (save-excursion
      (while (> (point) limit)
        (or (progn
              (skip-chars-backward gosh-reader--ws limit)
              (ignore-errors (gosh-parse--upward-sexp) t))
            (goto-char limit))
        (when (and (> (point) (point-min))
                   (eq ?\( (char-syntax (char-before)))
                   (eq ?w (char-syntax (char-after))))
          (setq scan-internal t)
          (let ((sym (gosh-parse-symbol-at-point)))
            (ignore-errors
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
                            (gosh-parse--extract-match-vars
                             (and (or (eq sym 'match) (< start limit)) start)
                             limit)))
                        vars)))
                ((let let* letrec let-syntax letrec-syntax
                      fluid-let and-let* do)
                 (or
                  (save-excursion
                    (gosh-parse--next-sexp)
                    (let* ((loop-name
                            (and (memq sym '(let))
                                 (eq ?w (char-syntax (char-after)))
                                 (prog1 (gosh-parse-symbol-at-point)
                                   (gosh-parse--next-sexp))))
                           (let-vars
                            (gosh-parse--let-vars-at-point
                             env let-limit loop-name)))
                      (if loop-name
                          ;; named let
                          (setq vars
                                (cons `(,loop-name (lambda ,(mapcar 'car let-vars)))
                                      (append let-vars vars)))
                        (setq vars (append let-vars vars))))
                    t)
                  (goto-char limit)))
                ;; TODO see match match-let*
                ;; TODO gosh-parse--extract-match-vars
                ((match-let1)
                 (save-excursion
                   (gosh-parse--next-sexp)
                   (let ((pat (gosh-read)))
                     ;;FIXME: too complecated
                     (setq vars
                           (append
                            (if (consp pat)
                                (loop for x on pat
                                      if (symbolp (car x))
                                      collect (list (car x)))
                              (list pat))
                            vars)))))
                ((let1 rlet1 if-let1)
                 (save-excursion
                   (gosh-parse--next-sexp)
                   (let ((sym (gosh-parse-symbol-at-point)))
                     (setq vars (cons `(,sym) vars)))))
                ((let-values let*-values)
                 (setq vars
                       (append (mapcar
                                'list
                                (gosh-append-map
                                 'gosh-flatten
                                 (gosh-filter 'consp
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
                                   (gosh-parse--read-definitions))))
                       (setq vars
                             (append (gosh-append-map
                                      (lambda (x)
                                        (and (consp (cdr x))
                                             (consp (cadr x))
                                             (eq 'lambda (caadr x))
                                             (mapcar
                                              (lambda (x)
                                                (cond
                                                 ((atom x)
                                                  (list x))
                                                 (t
                                                  x)))
                                              (gosh-flatten
                                               (cadadr x)))))
                                      defs)
                                     (and (not (= 1 (current-column))) defs)
                                     vars)))
                   (setq scan-internal nil)))))
            ;; check for internal defines
            (when scan-internal
              (ignore-errors
                (save-excursion
                  ;;TODO 3?
                  (forward-sexp 3)
                  (backward-sexp)
                  (when (< (point) start)
                    (setq vars (append (gosh-parse--read-inner-definitions) vars))))))))))
    (reverse vars)))

(defun gosh-parse--current-context ()
  (save-excursion
    (let ((res '()))
      (while (not (gosh-context-toplevel-p))
        (let ((end (point)))
          (gosh-beginning-of-list)
          (let* ((paren-start (char-after))
                 (paren-end (gosh-paren-against-char paren-start))
                 (s (buffer-substring (point) end))
                 (sexp (gosh-read-just-first (concat s `(,paren-end)))))
            (setq res (cons sexp res)))))
      res)))

(defun gosh-parse--current-sexp-in-list ()
  (save-excursion
    (gosh-end-of-string)
    (let ((syn0 (char-syntax (char-before)))
          (syn (char-syntax (char-after))))
      (when (or (memq syn0 '(?\s ?>))
                (not (memq syn '(?\s ?>))))
        (ignore-errors (forward-sexp)))
      (let* ((ctx (gosh-parse--current-context))
             (fnsexp (car-safe (last ctx)))
             (index (1- (length fnsexp))))
        (list fnsexp (max index 0))))))

(defun gosh-parse--current-context-for-opening (&optional count)
  (save-excursion
    (let ((c (or count 0))
          (start (point)))
      (while (and
              (not (gosh-context-toplevel-p)) ; top level
              (condition-case nil
                  (progn
                    (backward-sexp)
                    t)
                (scan-error
                 (cond
                  ((plusp c)
                   (decf c)
                   (skip-chars-backward "\s\t\n(["))
                  ((looking-at "\\(?:\\sw\\|\\s_\\)")
                   nil)
                  (t
                   (backward-char)
                   t))))))
      (skip-chars-backward "\s\t\n([")
      (let* ((partial (buffer-substring-no-properties (point) start))
             (parsed (parse-partial-sexp (point) start))
             (parenthese (mapcar
                          (lambda (x)
                            (gosh-paren-against-char (char-after x)))
                          (reverse (nth 9 parsed))))
             (closing (concat parenthese))
             (sexp (gosh-read-just-first
                    (concat partial " *" closing))))
        (and (consp sexp) sexp)))))

(defun gosh-parse--current-import-symbols (importers)
  (let ((res '()))
    (dolist (importer importers)
      (ignore-errors
        (let ((syms
               (let ((module (nth 0 importer))
                     (prefix (nth 1 importer)))
                 (mapcar
                  (lambda (d)
                    (cond
                     ((symbolp (car d))
                      (let* ((dname (symbol-name (car d)))
                             (fullsym (intern (concat prefix dname))))
                        (cons fullsym (cdr d))))
                     (t d)))
                  (gosh-cache-module-exports module)))))
          (setq res (append syms res)))))
    res))

(defun gosh-parse-gather-current-importer ()
  (let ((res '()))
    (save-excursion
      (goto-char (point-min))
      ;; scan for module forms
      (while (not (eobp))
        (ignore-errors
          (gosh-and* ((sexp (gosh-parse-read))
                      ((consp sexp)))
            (setq res
                  (append (gosh-parse--extract-importer sexp) res))))))
    res))

;;TODO require
(defun gosh-parse--extract-importer (sexp)
  (case (and (consp sexp) (car-safe sexp))
    ((begin define-module)
     (gosh-append-map 'gosh-parse--extract-importer (cdr sexp)))
    ((cond-expand)
     (gosh-append-map 'gosh-parse--extract-importer
                      (gosh-append-map 'cdr (cdr sexp))))
    ((use import)
     (let* ((module (nth 1 sexp))
            (gprefix (cadr (memq :prefix sexp)))
            (prefix (cond
                     ((and gprefix (symbolp gprefix))
                      (symbol-name gprefix))
                     ((stringp gprefix)
                      gprefix)
                     (t ""))))
       (list (list module prefix))))
    ((require-extension)
     (gosh-append-map 'gosh-parse--extract-importer (cdr sexp)))
    ((autoload)
     ;;TODO only arg is symbol
     (mapcar (lambda (x) (cons (if (consp x) (car x) x) '((lambda obj))))
             (cddr sexp)))))

(defun gosh-parse--construct-definition (sexp)
  (let ((fnsym (car-safe sexp))
        (body (cdr-safe sexp)))
    (case fnsym
      ((define-syntax)
       (let ((name (nth 0 body)))
         `((,name (syntax)))))
      ((define-macro)
       (let* ((sig (nth 0 body))
              (name (car sig))
              (args (cdr sig)))
         `((,name (syntax ,args)))))
      ((define-method)
       (let* ((name (car-safe body))
              (args (car (cdr-safe body))))
         `((,name (lambda ,args)))))
      ((define define-constant)
       (let* ((first (car-safe body)))
         (cond
          ((consp first)
           `((,(car first) (lambda ,(cdr first)))))
          ((and (atom first) (cdr body))
           `((,first ,(car-safe (cdr body))))))))
      ((define-class)
       (let ((name (nth 0 body))
             (super (nth 1 body))
             (slots (nth 2 body)))
         `((,name (class ,super ,slots)))))
      ((define-condition-type)
       (let ((name (nth 0 body))
             (super `(,(nth 1 body)))
             (slots (nthcdr 3 body)))
         `((,name (class ,super ,slots)))))
      ((define-record-type)
       (let ((name (nth 0 body))
             (members (nthcdr 3 body)))
         `((,name (class nil ,members)))))
      ((begin begin0)
       (gosh-append-map 'gosh-parse--construct-definition body))
      (t
       '()))))

(defun gosh-parse--read-definitions ()
  (save-excursion
    (let ((sexp (ignore-errors (gosh-read-list 5))))
      (gosh-parse--construct-definition sexp))))

;;TODO consider `gosh-parse-file-exports'
(defun gosh-parse-exports-functions (file)
  (gosh-with-find-file file
    (gosh-parse--current-exports)))

(defun gosh-parse-file-globals (file)
  (gosh-with-find-file file
    (save-excursion
      (goto-char (point-min))
      (gosh-parse--current-globals t))))

(defun gosh-parse-file-exports (file)
  (let (syms modules)
    (gosh-with-find-file file
      (setq syms (gosh-parse--exported-symbols))
      (setq modules (gosh-parse--current-base-modules)))
    (dolist (mod modules)
      (let ((f (gosh--module->file mod)))
        (setq syms (append (gosh-parse-file-exports f) syms))))
    syms))

;; for internal defines, etc.
;;TODO max
(defun gosh-parse--read-inner-definitions (&optional max)
  (let ((defs '())
        (end (or max (point-max))))
    (save-excursion
      (while (< (point) end)
        (let ((here (point))
              (new-defs (gosh-parse--read-definitions)))
          (cond
           (new-defs
            (setq defs (append new-defs defs))
            (or (ignore-errors
                  (gosh-parse--next-sexp)
                  (> (point) here))
                (goto-char end)))
           ;; non-definition form, maybe stop scanning
           (t
            (goto-char end))))))
    defs))

(defun gosh-parse--file-base-modules (file)
  (gosh-with-find-file file
    (gosh-parse--current-base-modules)))

(defun gosh-parse--current-base-modules ()
  (let (mod modules)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(extend\\_>" nil t)
        (when (gosh-context-code-p)
          (ignore-errors
            (while (setq mod (gosh-read))
              (setq modules (cons mod modules)))))))
    (nreverse modules)))

(defun gosh-parse--exported-symbols ()
  (let* ((env (gosh-parse--current-globals))
         (exports (gosh-parse--current-exports t env))
         (res '()))
    ;; if source file execute dynamic load.
    ;; global definition (env) will be null.
    (dolist (x exports)
      (setq res
            (cons (or (assq x env)
                      (cons x nil))
                  res)))
    ;; merge generic functions
    (dolist (x env)
      (when (memq (car x) exports)
        (unless (memq x res)
          (setq res (cons x res)))))
    res))

(defun gosh-parse--current-globals (&optional only-current)
  (let ((res '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((sexp (gosh-parse-read))
               (fnsym (car-safe sexp)))
          (case fnsym
            ((define-module)
             (unless only-current
               (let* ((body (cdr-safe sexp))
                      (parents (cdr (assq 'extend body))))
                 (setq res (append
                            (gosh-append-map
                             'gosh-cache-module-globals
                             parents)
                            res)))))
            ((extend)
             (unless only-current
               (let* ((body (cdr-safe sexp))
                      (parents (cdr body)))
                 (setq res (append
                            (gosh-append-map
                             'gosh-cache-module-globals
                             parents)
                            res)))))
            (t
             (setq res
                   (append
                    (ignore-errors (gosh-parse--construct-definition sexp))
                    res)))))))
    res))

(defun gosh-parse--current-exports (&optional only-current env)
  (let* ((res '())
         (extend-handler
          (lambda (form)
            (unless only-current
              (let ((parents (cdr form)))
                (setq res (append
                           (mapcar 'car
                                   (gosh-append-map
                                    'gosh-cache-module-exports
                                    parents))
                           res))))))
         (export-handler
          (lambda (form)
            (setq res (nconc (cdr form) res))))
         (export-all-handler
          (lambda ()
            (setq res (nconc
                       (mapcar
                        'car
                        (or env (gosh-parse--current-globals)))
                       res))
            ;; skip all
            (goto-char (point-max)))))

    (save-excursion
      (goto-char (point-min))
      (ignore-errors
        (while (not (eobp))
          (let* ((sexp (gosh-parse-read))
                 (fnsym (car-safe sexp)))
            (case fnsym
              ((define-module)
               (let ((decls (cddr sexp)))
                 (funcall extend-handler (assq 'extend decls))
                 (cond
                  ((and (consp decls) (assq 'export decls))
                   (funcall export-handler (assq 'export decls)))
                  ((and (consp decls) (assq 'export-all decls))
                   (funcall export-all-handler)))))
              ((export export-if-defined)
               (funcall export-handler (cdr sexp)))
              ((export-all)
               (funcall export-all-handler))
              ((extend)
               (funcall extend-handler sexp)))))))
    res))

(defun gosh-scheme-srfi-exports (i)
  (cond
   ((integerp i)
    (when (and (>= i 0)
               (< i (length *gosh-scheme-srfi-info*)))
      (cdr (aref *gosh-scheme-srfi-info* i))))))

(defun gosh-parse-current-env ()
  ;; r5rs
  (let ((env (list *gosh-scheme-r5rs-info*))
        (importers (gosh-parse-gather-current-importer)))
    ;;TODO gather applicants non imported module.
    ;; base language
    (let ((base (ignore-errors
                  (gosh-info-doc-env importers))))
      (when base
        (push base env)))
    ;; imports
    (let ((imports (ignore-errors
                     (gosh-parse--current-import-symbols importers))))
      (when imports
        (push imports env)))
    ;; top-level defs
    (let ((top (ignore-errors (gosh-parse--current-globals))))
      (when top
        (push top env)))
    ;; current local vars
    (let ((locals (ignore-errors (gosh-parse--current-local-vars env))))
      (when locals
        (push locals env)))
    env))

(defun gosh-parse-current-module ()
  (save-excursion
    (save-restriction
      (widen)
      (catch 'return
        ;; avoid starts of "(with-module"
        (when (eq (char-syntax (char-after)) ?\()
          (unless (bobp)
            (backward-char)))
        (let ((matcher
               (lambda ()
                 (when (looking-at "(\\(?:with\\|define\\)-module[\s\t\n]+\\([^\s\t\n()]+\\)")
                   (throw 'return (match-string-no-properties 1))))))
          (unless (gosh-context-toplevel-p)
            ;; while not top level
            (while (and (not (bobp))
                        (not (eq (char-syntax (char-after)) ?\()))
              (funcall matcher)
              (gosh-beginning-of-list))
            (funcall matcher)))
        (when (re-search-backward "^[\s\t]*(select-module[\s\t\n]+\\([^\s\t\n()]+\\)" nil t)
          (throw 'return (match-string-no-properties 1)))
        "user"))))

;;TODO consider cost of this code
(defun gosh-parse-read ()
  (let ((start (point)))
    (catch 'done
      (while (not (eobp))
        (condition-case nil
            (let ((sexp (gosh-read)))
              (throw 'done sexp))
          (error
           (goto-char start)
           (forward-line)
           ;;TODO nested comment
           (unless (re-search-forward "^(" nil t)
             ;;TODO nil?
             (throw 'done nil))
           (forward-line 0)
           (setq start (point))))))))

;;
;; Parsed cache
;;

;;TODO to have extend relation
(defvar gosh-cache--module-exports '()
  "Each item is following list
\\(file-name module-symbol time symbols-alist [defined-symbols])

TODO key should be module-file?? multiple executable make complex.
"
  )

;;TODO to have extend relation
;;TODO refactor file information collect to one object. same *-module-exports
(defvar gosh-cache--file-globals '()
  "Each item is following list
\\(file-name module-symbol time symbols-alist)"
  )

;; Gather export symbols of MOD.
(defun gosh-cache-module-exports (mod)
  (cond
   ((and (consp mod) (eq 'srfi (car mod)))
    ;; TODO remove this cond
    (gosh-append-map 'gosh-scheme-srfi-exports (cdr mod)))
   ((and (symbolp mod) (string-match "^srfi-\\([0-9]+\\)$" (symbol-name mod)))
    (let ((srfi-n (string-to-number (match-string 1 (symbol-name mod)))))
      (gosh-scheme-srfi-exports srfi-n)))
   (t
    (let* ((file (gosh--module->file mod))
           (cached (gosh-cache--find-with-mark file 'gosh-cache--module-exports)))
      (if cached
          (nth 3 cached)
        ;; (re)compute module exports
        (let ((res (gosh-parse-file-exports file)))
          (push (list file mod (gosh-file-mtime file) res)
                gosh-cache--module-exports)
          res))))))

;; Gather module all definitions which is derived by child module.
(defun gosh-cache-module-globals (mod)
  (let* ((file (gosh--module->file mod))
         (cached (gosh-cache--find-with-mark file 'gosh-cache--file-globals)))
    (if cached
        (nth 4 cached)
      ;; (re)compute module exports
      (let ((res (gosh-parse-file-globals file))
            (bases (gosh-parse--file-base-modules file)))
        (push (list file mod (gosh-file-mtime file) bases res)
              gosh-cache--file-globals)
        res))))

(defun gosh-cache--find-with-mark (file cache-var)
  (let* ((pred (lambda (item)
                 (equal (nth 0 item) file)))
         (cached (gosh-find pred (symbol-value cache-var))))
    (when (and cached
               (stringp (nth 0 cached))
               (ignore-errors
                 (let ((mtime (gosh-file-mtime (nth 0 cached)))
                       (ctime (nth 2 cached)))
                   (not (or (equal mtime ctime)
                            (time-less-p mtime ctime))))))
      (set cache-var (gosh-remove pred (symbol-value cache-var)))
      (setq cached nil))
    cached))


;;;;
;;;; General UI
;;;;

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

(defface gosh-modeline-lightdown-face
  '((t
     :foreground "White"))
  "Face used to unhighlight mode line."
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

(defun gosh--temp-message (format-string &rest args)
  (let (message-log-max)
    (apply 'message format-string args)))

(defmacro gosh--processing-message (message &rest form)
  (declare (indent 1))
  `(progn
     (gosh--temp-message ,message)
     ,@form
     (gosh--temp-message (concat ,message "done"))))


;;;
;;; momentary message
;;;

(defvar gosh-momentary-message--overlay nil)

(defface gosh-momentary-message-face
  '((((background dark))
     :foreground "LightSkyBlue" :bold t)
    (t
     :foreground "Blue1" :bold t))
  "Face used to momentary message."
  :group 'gosh-mode)

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
      (gosh-momentary-message--overlay start end)
      (sit-for wait-msec)
      (delete-region start end)
      (set-buffer-modified-p modified)
      (delete-overlay gosh-momentary-message--overlay)
      (when quit-flag
        (setq quit-flag nil)
        (setq unread-command-events (list 7))))))

(defun gosh-momentary-message--overlay (start end)
  (let ((ov gosh-momentary-message--overlay))
    (unless ov
      (setq ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face 'gosh-momentary-message-face)
    (move-overlay ov start end (current-buffer))
    (setq gosh-momentary-message--overlay ov)))


;;;
;;; eldoc
;;;

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
         (gosh-eldoc--initialize-rotate-timer))
  :initialize (lambda (s v) (set s v)))

(defun gosh-eldoc--initialize ()
  (when (require 'eldoc nil t)
    (when (numberp gosh-eldoc-idle-delay)
      (set (make-local-variable 'eldoc-idle-delay) gosh-eldoc-idle-delay))
    (set (make-local-variable 'eldoc-documentation-function)
         'gosh-eldoc--print-current-symbol-info)
    (gosh-eldoc--initialize-rotate-timer)))

(defun gosh-eldoc--initialize-rotate-timer ()
  (unless gosh-eldoc--rotate-timer
    (setq gosh-eldoc--rotate-timer
          ;; eldoc-rotate must be delay than eldoc
          (run-with-timer (* eldoc-idle-delay 2)
                          gosh-eldoc-rotate-seconds
                          'gosh-eldoc--rotate-print-info))))

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
    ;; index exceed maximum but
    ;; * (lambda args)
    ;; * (lambda (:rest args))
    (unless target-exp
      (gosh-if-let1 it (gosh-eldoc--sexp-rest-arg real-sexp)
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
    (dolist (exp sexp)
      (cond
       ((eq exp :key)
        (setq ignore t))
       ((eq exp :optional)
        (setq ignore nil))
       (ignore )
       (t
        (setq ret (cons exp ret)))))
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
    (dolist (x sexp)
      (when (and (vectorp x)
                 (> (length x) 0)
                 (let ((sym (aref x 0)))
                   (and (symbolp sym)
                        (string-match "\\.\\.\\.$" (symbol-name sym)))))
        (throw 'found x)))
    ;; vector SEXP to list
    (let ((last (car (last (append sexp nil)))))
      (when (and last
                 (symbolp last)
                 (string-match "\\.\\.\\.$" (symbol-name last)))
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

(defun gosh-eldoc--rotate-print-info ()
  (save-match-data
    (with-local-quit
      (condition-case err
          (when (eldoc-display-message-p)
            (when (eq (aref gosh-eldoc--cached-data 0) (point))
              (let* ((index (aref gosh-eldoc--cached-data 1))
                     (info (aref gosh-eldoc--cached-data 2))
                     (len (length info)))
                (when (> len 1)
                  (aset gosh-eldoc--cached-data 1 (% (1+ index) len))
                  (eldoc-message (nth index info))))))
        (error (message "gosh-eldoc error: %s" err))))))

(defun gosh-eldoc--print-current-symbol-info ()
  (save-match-data
    (with-local-quit
      (when (gosh-context-code-p)
        (let* ((fnsym0 (gosh-parse--current-sexp-in-list))
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
               (strings (delq nil (append string1 string2))))
          (gosh-eldoc--cache-set (point) 0 strings)
          (car strings))))))

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
      (dolist (x (cdr obj))
        (setq ret (cons (gosh-eldoc--elisp->scheme-string x) ret)))
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
   ((and (consp spec) (consp (cdr spec)))
    (let ((type (nth 1 spec)))
      (concat
       (cond
        ((nth 3 spec) "")               ;TODO ?? super? slots?
        ((and (consp type) (eq (car type) 'class))
         (concat "Class: "
                 (gosh-eldoc--sexp->string (car spec))
                 ;;super class
                 (when (cadr type)
                   (concat
                    " Base: "
                    (gosh-eldoc--sexp->string (cadr type))))
                 (when (caddr type)
                   (concat
                    " Slots: "
                    (gosh-eldoc--sexp->string
                     (mapcar 'car-safe (caddr type)))))))
        ((and (consp type) (memq (car type) '(syntax lambda)))
         (concat
          (and (eq (car type) 'syntax)
               "Syntax: ")
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
        ((stringp type)
         (concat
          "String: \""
          (gosh-eldoc--sexp->string (cdr type))
          "\""))
        ((numberp type)
         (concat
          "Number: "
          (gosh-eldoc--sexp->string type)))
        ((symbolp type)
         (let ((spec (gosh-env-lookup env type)))
           ;; ignore `max-lisp-eval-depth' error
           (or (gosh-eldoc--find-and-print-string spec env highlight)
               (format "some parameter or alias typed `%s'" type))))
        ((gosh-object-p type)
         (concat
          (capitalize (symbol-name (gosh-object-type type)))
          ": "
          (gosh-eldoc--sexp->string (gosh-object-value type))))
        (t
         (gosh-eldoc--sexp->string type)))
       (if (and (not (nth 3 spec)) (nth 4 spec)) " - " "")
       (or (nth 4 spec) ""))))
   ((and (consp spec) (null (cdr spec)))
    "Some of ( Parameter | Alias | Dynamic loaded procedure )")))


;;;
;;; Completion
;;;

(defun gosh-completion-do (str coll &optional strs pred)
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
     ((null completion)
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
        cands tmp)
    (dolist (l ls)
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
        res s)
    ;; find most detailed info
    (dolist (s specs)
      (when (and (consp s)
                 (or (> (length s) (length res))
                     (and (= (length s) (length res))
                          ;;FIXME TODO....
                          (consp (cadr s))
                          (consp (cadr res))
                          (eq 'lambda (caadr s))
                          (eq 'lambda (caadr res))
                          ;; compare lambda args
                          (> (length (gosh-flat (cadadr s)))
                             (length (gosh-flat (cadadr res)))))))
        (setq res s)))
    res))

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
                        (gosh-find
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
                       (gosh-find
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

;;TODO name
(defun gosh-param-list-match-p (p1 p2)
  (or (and (symbolp p1) (not (null p1)))
      (and (symbolp p2) (not (null p2)))
      (and (null p1) (null p2))
      (and (consp p1) (consp p2)
           (gosh-param-list-match-p (cdr p1) (cdr p2)))))

;;TODO name
(defun gosh-translate-special-type (x)
  (if (not (consp x))
      x
    (case (car x)
      ((list string) (car x))
      ((set special) (cadr x))
      ((flags) 'integer)
      (t x))))

(defun gosh-complete-symbol-name-at-point ()
  (let ((end (point))
        (start (save-excursion
                 (skip-syntax-backward "w_")
                 (point))))
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
    (gosh-filter 'file-directory-p res2)))

(defun gosh-complete-in-string (type)
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
       (car (delete nil (mapcar 'gosh-complete-in-string (cdr type)))))))))

(defun gosh-complete-apply-string-completer (cmpl sym)
  (let ((func (if (consp cmpl) (car cmpl) cmpl))
        (trans (and (consp cmpl) (cadr cmpl))))
    (funcall func trans sym)))

(defun gosh-smart-complete (&optional arg)
  (interactive "P")
  (let* ((sym (gosh-complete-symbol-name-at-point))
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
      (gosh-completion-do sym (gosh-env-filter (lambda (x) t) env)))
     ;; allow different types of strings
     (in-str-p
      (let* ((param-type
              (and (consp inner-type)
                   (eq 'lambda (car inner-type))
                   (gosh-lookup-type (cadr inner-type) inner-pos)))
             (completer (or (gosh-complete-in-string param-type)
                            '(gosh-complete-file-name
                              file-name-nondirectory))))
        (gosh-completion-do
         ;;(if (consp completer) (funcall (cadr completer) sym) sym)
         sym
         (gosh-complete-apply-string-completer completer sym))))
     ;; outer special
     ((and (consp outer-type)
           (eq 'special (car outer-type))
           (cadddr outer-type))
      (gosh-completion-do sym (funcall (cadddr outer-type) sym)))
     ;; inner special
     ((and (consp inner-type)
           (eq 'special (car inner-type))
           (caddr inner-type))
      (gosh-completion-do sym (funcall (caddr inner-type) sym)))
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
        (gosh-completion-do
         sym
         (gosh-env-filter
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
              (gosh-env-filter
               (lambda (x)
                 (and (not (and (consp (cadr x)) (eq 'syntax (caadr x))))
                      (gosh-type-match-p (cadr x) base-type)))
               env))
             (str-completions
              (let ((completer (gosh-complete-in-string base-type)))
                (and
                 completer
                 (gosh-complete-apply-string-completer completer sym)))))
        (gosh-completion-do
         sym
         (append set-or-flags base-completions)
         str-completions)))
     ;; completing a function
     ((zerop inner-pos)
      (gosh-completion-do
       sym
       (gosh-env-filter
        (lambda (x)
          (or (null (cdr x))
              (and (cadr x) (atom (cadr x)))
              (memq (cadr x) '(procedure object nil))
              (and (consp (cadr x))
                   (memq (caadr x) '(lambda syntax special)))))
        env)))
     ;; complete everything
     (t
      (gosh-completion-do sym (gosh-env-filter (lambda (x) t) env))))))

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

(defun gosh-test-module ()
  "Execute test for module which is cursor indicated to."
  (interactive)
  (let ((module (gosh-test--update-module)))
    (gosh-test--invoke-process module)))

(defun gosh-test-popup-result (&optional all)
  "Show test result if there is."
  (interactive "P")
  (cond
   ((null (gosh-mode-get :modeline-test-result))
    (message "Current module is not tested yet."))
   ((not all)
    (let ((msgs (loop for o in (overlays-at (point))
                      if (overlay-get o 'flymake-overlay)
                      collect (overlay-get o 'help-echo))))
      (cond
       (msgs
        (ding)
        (message "Test Error: %s %s"
                 (mapconcat 'identity msgs ", ")
                 (propertize "(This mark possiblly point to the wrong place.)"
                             'face 'font-lock-warning-face)))
       (t
        (message "No error at point.")))))
   (t
    (let ((msg (cadr (memq 'help-echo (gosh-mode-get :modeline-test-result)))))
      (cond
       (msg
        (ding)
        (message "%s" msg))
       (t
        (message "No error.")))))))


;;;
;;; auto-complete
;;;

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
          (prefix . "[\s\t\n]\\(\\(?:\\sw\\|\\s_\\)+\\)")
          (requires . 2)
          (cache)))

      (ac-define-source gosh-keywords
        '((candidates . gosh-ac-keywords-candidates)
          (symbol . "k")
          (prefix . "[\s\t\n]\\(:\\(?:\\sw\\|\\s_\\)*\\)")
          (requires . 1)
          (cache)))

      (let ((syntaxes '("use" "import" "select-module"
                        "with-module" "extend" "define-in-module")))
        (ac-define-source gosh-modules
          `((candidates . gosh-ac-module-candidates)
            (symbol . "m")
            (prefix . ,(concat "(" (regexp-opt syntaxes) "[\s\t\n]+\\(\\(?:\\sw\\|\\s_\\)+\\)"))
            (requires . 1)
            (cache))))

      (ac-define-source gosh-inferior-symbols
        '((candidates . gosh-ac-inferior-candidates)
          (symbol . "s")
          (requires . 2)
          (cache)))

      (add-to-list 'ac-modes 'gosh-mode)
      (add-to-list 'ac-modes 'gosh-inferior-mode))))

(defun gosh-ac--initialize-for-mode ()
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
  (let ((fnsym (gosh-parse--current-sexp-in-list)))
    (when (and (cadr fnsym) (> (cadr fnsym) 0))
      (let ((fn (caar fnsym))
            (env (gosh-parse-current-env)))
        (cond
         ((eq fn 'make)
          (gosh-ac-keywords-class-slot-candidates fnsym env))
         (t
          (gosh-ac-keywords-generic-candidates fn fnsym env)))))))

(defun gosh-ac-keywords-class-slot-candidates (fnsym env)
  (let ((class (cadar fnsym))
        res)
    (when class
      (gosh-map-env-symbols env c
        (when (and (consp c)
                   (consp (cadr c))
                   (eq (caadr c) 'class)
                   (eq class (car c)))
          (let ((slots (car (cddadr c))))
            (mapcar
             (lambda (k)
               (cond
                ((atom k)
                 (setq res (cons (gosh-ac-symbol->keyword-string k) res)))
                ((memq :init-keyword k)
                 (setq res (cons (gosh-ac-symbol->keyword-string
                                  (cadr (memq :init-keyword k)))
                                 res)))
                ((car k)
                 (setq res (cons
                            (gosh-ac-symbol->keyword-string (car k))
                            res)))))
             slots))))
      res)))

(defun gosh-ac-keywords-generic-candidates (fn fnsym env)
  (let (res)
    (gosh-map-env-symbols env c
      (when (and (eq (car c) fn)
                 (consp (cadr c))
                 (memq (caadr c) '(lambda syntax)))
        (let ((key-args (cdr (member :key (cadadr c)))))
          (mapcar (lambda (k)
                    (setq res (cons
                               (gosh-ac-symbol->keyword-string
                                (if (consp k) (car k) k))
                               res)))
                  key-args))))
    res))

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


;;;
;;; Parenthese / Bracket handling (from quack)
;;;

(defun gosh-closing--balance-all-paren ()
  "Balance all closing parenthese"
  (let ((state (gosh-paren--current-status)))
    (when (eq state 'balanced)
      (let (start (end (point)))
        (save-excursion
          (backward-sexp)
          (setq start (point))
          (save-restriction
            (narrow-to-region start end)
            ;; search opening paren
            (while (re-search-forward "[[(]" nil t)
              (when (and (gosh-context-code-p)
                         ;; escaped parenthese
                         (not (eq (char-before (1- (point))) ?\\)))
                (let* ((opening (char-after (1- (point))))
                       (end (gosh--scan-sexps (1- (point)) 1))
                       (actual-close (char-before end))
                       (closing (gosh-paren-against-char opening)))
                  (unless (eq actual-close closing)
                    (save-excursion
                      (goto-char end)
                      (delete-char -1)
                      (insert closing))))))))))))

(defun gosh-closing--insert (force default-close)
  (insert default-close)
  (unless force
    (let ((start (gosh--scan-sexps (point) -1)))
      (cond
       ((not (gosh-context-code-p)))
       ((not start)
        (beep))
       (t
        (let* ((opening (gosh-paren-against-char default-close))
               (actual-open (char-after start))
               (closing (gosh-paren-against-char actual-open)))
          (unless (eq actual-open opening)
            (delete-region (1- (point)) (point))
            (insert closing)))
        (gosh-closing--balance-all-paren)))))
  (when blink-paren-function
    (funcall blink-paren-function)))

(defun gosh-closing-insert-paren (&optional force)
  "Close opening parenthese or bracket.
Arg FORCE non-nil means forcely insert parenthese."
  (interactive "P")
  (gosh-closing--insert force ?\)))

(defun gosh-closing-insert-bracket (&optional force)
  "Close opening parenthese or bracket.
Arg FORCE non-nil means forcely insert bracket."
  (interactive "P")
  (gosh-closing--insert force ?\]))

(defun gosh-paren--current-status ()
  (save-excursion
    (let ((first (point))
          (next nil))
      (cond
       ;; goto the current toplevel
       ((not (re-search-backward "^(" nil t))
        'unexpected)
       (t
        (let ((s (point)))
          (forward-char)
          ;; goto next toplevel or end of buffer
          (or
           (progn
             (when (re-search-forward "^(" nil t)
               (backward-char)
               t))
           (goto-char (point-max)))
          ;;TODO check parse-partial-sexp doc
          (let ((part (parse-partial-sexp s (point))))
            (cond
             ((zerop (car part)) 'balanced)
             ((minusp (car part)) 'unbalanced)
             (t 'opening)))))))))

(defun gosh-opening--insert (force default-open)
  (insert default-open)
  ;; status after insert open char.
  (let ((state (gosh-paren--current-status)))
    (when force
      (cond
       ((eq state 'balanced)
        (save-excursion
          (backward-char)
          (let* ((end (gosh--scan-sexps (point) 1))
                 (close (char-before end))
                 (against (gosh-paren-against-char default-open)))
            (unless (eq against close)
              (goto-char end)
              (delete-char -1)
              (insert against)
              ;; notify user change closing.
              (sit-for 0.2)))))))
    (unless force
      (cond
       ((not (gosh-context-code-p)))
       ((eq state 'balanced)
        ;; insert and backward with checking paren
        (backward-char)
        (gosh-opening--switch-paren)
        ;; move to after inserted point
        (forward-char))
       ((eq state 'opening)
        (when (save-excursion
                (backward-char)
                (when (gosh-opening--bracket-p)
                  (delete-char 1)
                  (insert "[")
                  t))
          (forward-char)))
       ((eq state 'unbalanced)
        ;; do nothing
        )))
    (when blink-paren-function
      (save-excursion
        (backward-char 1)
        (and (let ((close-pt (gosh--scan-sexps (point) 1)))
               (when close-pt
                 (goto-char close-pt)))
             ;; notify if scan-sexps succeed and unmatched parenthese
             (funcall blink-paren-function))))))

(defun gosh-opening--switch-paren ()
  (let ((open (char-after))
        (next (gosh--scan-sexps (point) 1)))
    (when (and next (eq (char-syntax open) ?\())
      (let* ((close (char-before next))
             (against (gosh-paren-against-char close)))
        (unless (eq against open)
          (delete-char 1)
          (insert against)
          (backward-char))))))

(defun gosh-opening-insert-paren (&optional force)
  "Insert opening paren and notify if there is unmatched bracket."
  (interactive "P")
  (gosh-opening--insert force ?\())

(defun gosh-opening-insert-bracket (&optional force)
  "Insert opening bracket and notify if there is unmatched parenthese."
  (interactive "P")
  (gosh-opening--insert force ?\[))

;; TODO move to gosh-config
(defvar gosh-opening--auto-bracket-alist
  '(
    (fluid-let (*))
    (do (*))
    (let (*))
    (let* (*))
    (let gosh-symbol-p (*))             ; named let
    (letrec (*))
    (and-let* (*))
    (case t *)
    (ecase t *)
    (cond *)
    (cond-list *)
    (guard (gosh-symbol-p *))
    (syntax-rules t *)

    (match t *)
    (match-lambda *)
    (match-lambda* *)
    (match-let (*))
    (match-let t (*))
    (match-let* (*))
    (match-letrec (*))

    (rxmatch-case t *)
    (rxmatch-cond t *)

    (let-args t (*))
    ))

;; `*' point to the cursor position.
(defun gosh-opening--context-bracket-p (context)
  (let (match-to)
    (let ((match-to
          (lambda (def args)
            (loop for a1 in def
                  for a2 on args
                  if (eq a1 '*)
                  return (member a1 a2)
                  else if (listp a1)
                  return (funcall match-to a1 (car a2))
                  else if (and (functionp a1)
                               (not (funcall a1 (car a2))))
                  return nil))))
      (let ((proc (car context)) (args (cdr context)))
        (loop for (def-name . def-args) in gosh-opening--auto-bracket-alist
              if (and (eq def-name proc)
                      (funcall match-to def-args args))
              return t)))))

(defun gosh-opening--bracket-p ()
  ;; retry 5 count backward current sexp
  (loop with context
        for i from 0 to 5
        do (setq context (gosh-parse--current-context-for-opening i))
        if (and context (gosh-opening--context-bracket-p context))
        return t))


;;;
;;; indentation handling
;;;

(defvar calculate-lisp-indent-last-sexp)

;; Copied from scheme-indent-function, but ignore
;; scheme-indent-function property if indent target is guessed as 
;; a local variable.
;; To use this function
;;  (set (make-local-variable 'lisp-indent-function) 'gosh-smart-indent)
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
             (method (and (not (assq function-sym (gosh-parse--current-local-vars)))
                          (get function-sym 'scheme-indent-function)))
             indent)
        (cond
         ((setq indent (gosh--smart-indent-assoc-symbol function-sym function))
          (lisp-indent-specform indent state indent-point normal-indent))
         ((or (eq method 'defun)
              (and (null method)
                   (> (length function) 3)
                   (string-match "\\`def" function)))
          (lisp-indent-defform state indent-point))
         ((integerp method)
          (lisp-indent-specform method state
                                indent-point normal-indent))
         (method
          (funcall method state indent-point normal-indent)))))))

(defvar gosh--smart-indent-alist nil
  "Pseudo EBNF is below.

ALIST ::= { MODULE | PROCEDURE }
MODULE ::= MODULE-SYMBOL , { PROCEDURE }
PROCEDURE ::= REGEXP , LEVEL | PROCEDURE-SYMBOL , LEVEL

LEVEL ::= number
REGEXP ::= string
MODULE-SYMBOL ::= symbol
PROCEDURE-SYMBOL ::= symbol
")

(defun gosh-smart-indent-rule (procedure-symbol-or-regexp level &optional module)
  (let ((sym-or-reg procedure-symbol-or-regexp))
    (catch 'done
      (dolist (x gosh--smart-indent-alist)
        (cond
         ((not (consp x)))
         ((and (null module)
               (atom (cdr x)))
          (cond
           ((and (symbolp (car x))
                 (eq sym-or-reg (car x)))
            (setcdr x level)
            (throw 'done x))
           ((and (stringp (car x))
                 (equal sym-or-reg (car x)))
            (setcdr x level)
            (throw 'done x))))
         ((and module
               (listp (cdr x))
               (eq module (car x)))
          (let ((pair (assoc sym-or-reg (cdr x))))
            (cond
             (pair
              (setcdr pair level))
             (t
              (setq pair (cons sym-or-reg level))
              (setcdr x (cons pair (cdr x)))))
            (throw 'done pair)))))
      ;; not found
      (let ((procedure (cons sym-or-reg level)))
        (setq gosh--smart-indent-alist
              (cons
               (if module
                   (cons module (list procedure))
                 procedure)
               gosh--smart-indent-alist))
        procedure))))

(defun gosh--smart-indent-assoc-symbol (symbol name &optional in-module alist)
  (let ((importers (or in-module (gosh-parse-gather-current-importer)))
        (module (intern (gosh-parse-current-module)))
        (alist (or alist gosh--smart-indent-alist)))
    (catch 'found
      (dolist (x alist)
        (cond
         ((not (consp x)))
         ((and (symbolp (car x))
               (numberp (cdr x)))
          ;; car is symbol name
          (when (eq symbol (car x))
            (throw 'found (cdr x))))
         ((and (stringp (car x))
               (numberp (cdr x)))
          ;; car is regexp
          (when (string-match (car x) name)
            (throw 'found (cdr x))))
         ((and (symbolp (car x))
               (listp (cdr x)))
          (dolist (importer importers)
            (let* ((mod (nth 0 importer))
                   (prefix (nth 1 importer))
                   (res
                    (cond
                     ((null importer))
                     ((eq module (car x))
                      (gosh--smart-indent-assoc-symbol symbol name t (cdr x)))
                     ((not (eq mod (car x))) nil)
                     ((string= prefix "")
                      (gosh--smart-indent-assoc-symbol symbol name t (cdr x)))
                     ((gosh-string-starts-with prefix name)
                      (let* ((name2 (substring name (length prefix)))
                             (symbol2 (intern-soft name2)))
                        (gosh--smart-indent-assoc-symbol
                         symbol2 name2 t (cdr x))))
                     (t nil))))
              (when res
                (throw 'found res)))))))
      ;; not found
      nil)))


;;;
;;; info integration
;;;

(defun gosh-info-lookup-add-help (mode)
  (info-lookup-add-help
   ;; For
   ;;  info-complete-symbol (to complete a symbol using the info)
   ;;  info-lookup-symbol   (to look up a symbol in the info)
   :topic 'symbol
   :mode  mode
   :regexp "[^()'\"\s\t\n]+"
   :ignore-case nil
   :doc-spec gosh-info-appendixes
   :parse-rule  nil
   :other-modes nil))

(defun gosh-info-lookup-initialize ()
  (gosh-info-lookup-add-help 'gosh-mode)
  (gosh-info-lookup-add-help 'gosh-inferior-mode))

(defun gosh-info--non-documented-modules ()
  (loop with documented = (gosh-info--documented-modules)
        for m in (gosh-available-modules)
        unless (member m documented)
        collect m))

(defun gosh-info--documented-modules ()
  (loop for e in gosh-info-doc--env
        if (and (car-safe e)
                (symbolp (car-safe e)))
        collect (symbol-name (car-safe e))))

(defun gosh-info-doc-initialize ()
  (setq gosh-info-doc--env
        (gosh-info-doc--generate-signatures)))

(defun gosh-info-doc-env (importers)
  (gosh-append-map
   (lambda (e)
     (let ((maybe-module (car-safe e)))
       (cond
        ((symbolp maybe-module)
         (gosh-and*
             ((importer (assq maybe-module importers))
              (prefix (nth 1 importer))
              (env (cdr-safe e)))
           (cond
            ((string= prefix "")
             env)
            (t
             (mapcar
              (lambda (x)
                (let* ((sym (car-safe x))
                       (body (cdr-safe x))
                       (fullnm (format "%s%s" prefix sym)))
                  (cons (intern fullnm) body)))
              env)))))
        ;; This env is not depend on any module
        (t e))))
   gosh-info-doc--env))

;;TODO gosh-info-doc
(defun gosh-info-doc-guess-module (symbol)
  (loop for e in gosh-info-doc--env
        if (assq symbol (cdr-safe e))
        return (symbol-name (car-safe e))))

(defvar gosh-info-doc--env nil)
(defun gosh-info-doc--generate-signatures ()
  (gosh-and*
   ((infofile (ignore-errors (Info-find-file "gauche-refe")))
    (dir (file-name-directory infofile))
    (filename (file-name-nondirectory infofile))
    (files (directory-files dir t (concat "\\`" filename)))
    (info-index (lambda (fn)
                  (or
                   (and (string-match "-\\([0-9]+\\)?\\(?:\\.gz\\)?\\'" fn)
                        (string-to-number (match-string 1 fn)))
                   0)))
    (sorted-files (sort files
                        (lambda (x y)
                          (< (funcall info-index x) (funcall info-index y))))))
   (with-temp-buffer
     (dolist (f sorted-files)
       (goto-char (point-max))
       (insert-file-contents f))
     (gosh-info-doc--parse-buffer))))

(defun gosh-info-doc--parse-buffer ()
  (goto-char (point-min))
  (let ((res '())
        (in-module '()))
    (while (re-search-forward "^ -- \\([^:]+\\): " nil t)
      (let ((cat (downcase (match-string 1))))
        (when (looking-at "\\(\\(?:.*\\)\\(?:\n\s\\{10\\}[^\s].*\\)*\\)")
          (let* ((sig-text (match-string 1))
                 (signature (gosh-info-doc--text-to-signature cat sig-text)))
            (when signature
              (cond
               ((equal cat "module")
                (when in-module
                  (setq res (cons (nreverse in-module) res)))
                (setq in-module (cons signature nil)))
               (t
                (cond
                 ;; expand ^a - ^z special lambda
                 ((eq (car-safe signature) '^c)
                  (setq in-module (append 
                                   (loop for c downfrom ?z downto ?a
                                         collect (let ((sym (intern (format "^%c" c))))
                                                   (cons sym (cdr-safe signature))))
                                   in-module)))
                 (t
                  (setq in-module (cons signature in-module)))))))))))
    (when in-module
      (setq res (cons (nreverse in-module) res)))
    (nreverse res)))

(defun gosh-info-doc--text-to-signature (cat text)
  (let ((res '()))
    (condition-case nil
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (while (not (eobp))
            (condition-case nil
                (let ((sexp (gosh-read)))
                  (setq res (cons sexp res)))
              (end-of-buffer)))
          (setq res (nreverse res)))
      (error
       ;;FIXME: 
       ;; (logger-info "%s: %s" cat text)
       nil))
    (cond
     ((null res) res)
     ((member cat '("function" "method" "generic function"))
      `(,(car-safe res) (lambda ,(cdr-safe res))))
     ((member cat '("macro" "special form"))
      `(,(car-safe res) (syntax ,(cdr-safe res))))
     ((member cat '("class" "condition type" "builtin class"))
      ;; TODO slots and super
      `(,(car-safe res) (class nil nil)))
     ((member cat '("ec qualifier"))
      ;;TODO
      ;; (list cat res)
      nil)
     ((member cat '("module"))
      (car-safe res))
     ((member cat '("constant" "variable" "parameter"))
      ;;TODO
      `(,(car-safe res) ,(car-safe (cdr-safe res))))
     ((member cat '("program clause" "command option"
                    "environment variable" "reader syntax"
                    "record type" "generic application"
                    "builtin module"))
      nil)
     (t nil))))

(defun gosh-info-initialize ()
  (require 'info)
  (info-initialize)
  (gosh-info-doc-initialize)
  (gosh-info-lookup-initialize))


;;;
;;; test functions
;;;

(defvar gosh-test--modeline-format
  `(
    " "
    ;; load status before test-module
    (:eval (gosh-mode-get :modeline-load-status))
    " "
    (:eval (or (gosh-mode-get :modeline-tested-module)
               ;; show a current module
               (gosh-mode-get :current-module)))
    "=>"
    ;; result of test-module
    (:eval (gosh-mode-get :modeline-test-result)))
  "Format for displaying the function in the mode line.")
(put 'gosh-test--modeline-format 'risky-local-variable t)

(defconst gosh-test--message-alist
  '(
    ("found dangling autoloads:" "Dangling autoloads")
    ("symbols exported but not defined:" "Exported but not defined")
    ("symbols referenced but not defined:" "Not defined")
    ("procedures received wrong number of argument:" "Wrong number of arguments")
    ))

(defun gosh-test--buffer-was-changed-p ()
  "Return non-nil if current-buffer was changed after validated"
  (or (null (gosh-mode-get :modtime))
      (null (gosh-mode-get :test-time))
      (> (gosh-mode-get :modtime)
         (gosh-mode-get :test-time))))

(defun gosh-test--parse-results (errors)
  ;; not exactly correct
  (loop with res = nil
        for (msg show) in gosh-test--message-alist
        do (loop for err in errors
                 if (string-match (format "%s \\(.+?\\)\\(?: AND \\|$\\)" msg) err)
                 do (let* ((line (match-string 1 err))
                           (syms (split-string line "[, ]" t)))
                      (setq res (append (gosh-test--parse-symbols show syms) res))))
        finally return res))

(defun gosh-test--parse-symbols (msg symbols)
  (delq nil
        (mapcar
         (lambda (x)
           (cond
            ((string-match "^(\\([^)]+\\))$" x)
             (list msg (match-string 1 x) nil))
            ((string-match "^\\([^(]+\\)(+\\([^)]+\\))?$" x)
             (list msg (match-string 2 x) (match-string 1 x)))
            (t nil)))
         symbols)))

(defun gosh-test--highilght-errors-if-possible (errors)
  "Highlight ERRORS as much as possible."
  (save-excursion
    (loop for (msg gsym lsym) in (gosh-test--parse-results errors)
          do (let ((greg (regexp-quote gsym))
                   (lreg (and lsym (regexp-quote lsym))))
               ;; search backward. last definition is the test result. maybe...
               (goto-char (point-max))
               (when (re-search-backward (format "^(def.+?\\_<\\(%s\\)\\_>" greg) nil t)
                 (let ((beg (match-beginning 1))
                       (fin (match-end 1)))
                   (when (and lreg
                              (re-search-forward (format "\\_<\\(%s\\)\\_>" lreg) nil t))
                     (setq beg (match-beginning 1))
                     (setq fin (match-end 1)))
                   (condition-case err
                       (gosh-test--make-error
                        beg fin msg 'flymake-errline)
                     (error
                      (gosh-test--make-error
                       beg fin msg 'flymake-errline)))))))))

(defun gosh-test--make-error (beg end tooltip-text face)
  "Allocate a flymake overlay in range BEG and END."
  ;;TODO
  (when (not (flymake-region-has-flymake-overlays beg end))
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face face)
      (overlay-put ov 'help-echo tooltip-text)
      (overlay-put ov 'flymake-overlay t)
      (overlay-put ov 'priority 100)
      (overlay-put ov 'evaporate t)
      ;;TODO what is fringe
      ;; (overlay-put ov 'before-string 'fringe)
      ov)))

(defun gosh-test--update-modeline ()
  (let* ((prev-module (gosh-mode-get :tested-module))
         (module (gosh-test--update-module)))
    (when (or (gosh-test--buffer-was-changed-p)
              (not (equal prev-module module)))
      (gosh-test--reset-result)))
  (force-mode-line-update))

(defun gosh-test--update-module ()
  "Refresh :current-module and return that new value."
  (let ((module (gosh-parse-current-module)))
    (gosh-mode-put :current-module module)
    module))

(defun gosh-test--reset ()
  (gosh-test--reset-result)
  (gosh-test--reset-status))

(defun gosh-test--reset-status ()
  (gosh-mode-put :modeline-load-status
    '(:propertize "Unknown" face gosh-modeline-lightdown-face)))

(defun gosh-test--reset-result ()
  (gosh-mode-put :modeline-tested-module nil)
  (gosh-mode-put :modeline-test-result
    '(:propertize "Unknown" face gosh-modeline-lightdown-face)))

;; Execute subprocess that have sentinel and filter
;;  subprocess load current file and evaluate `(test-module some-module)'
;;  -> sentinel read test-module result
(defun gosh-test--invoke-process (module)
  (let ((test-buffer (current-buffer))
        (file (gosh-mode--temp-file))
        (result-buf (generate-new-buffer " *gosh-mode-test* "))
        proc)
    ;; clear overlay
    (gosh-test--clear-errors)
    (gosh-mode-put :modeline-test-result
      `(:propertize "Checking"
                    face gosh-modeline-working-face))
    (gosh-mode-put :modeline-load-status
      '(:propertize "Checking" face gosh-modeline-working-face))
    (gosh-mode-put :test-time (float-time))
    (setq proc (start-process "Gosh test" result-buf
                              gosh-default-command-internal
                              "-i" "-u" "gauche.test"
                              "-l" file
                              "-e" (format "(test-module '%s)" module)))
    (set-process-sentinel proc 'gosh-test--process-sentinel)
    (set-process-filter proc 'gosh-test--process-filter)
    (process-put proc 'gosh-test-source-buffer test-buffer)
    ;; to terminate gosh process after evaluate "-e"
    (process-send-eof proc)
    (gosh-mode-put :modeline-tested-module
      (and module `(:propertize ,module face gosh-modeline-normal-face)))
    (gosh-mode-put :tested-module module)
    proc))

(defun gosh-test--process-filter (proc event)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (let ((buffer (process-get proc 'gosh-test-source-buffer)))
        (if (not (buffer-live-p buffer))
            (gosh-test--close-process proc)
          (goto-char (point-max))
          (insert event)
          (goto-char (point-min))
          ;; search error message
          (cond
           ((re-search-forward "^gosh: \\(.*\\)\n" nil t)
            (let ((message (match-string 1)))
              (with-current-buffer buffer
                (gosh-test--parse-error-message message)
                (gosh-mode-put :modeline-load-status
                  `(:propertize "Invalid"
                                face gosh-modeline-error-face
                                help-echo ,(concat "Gosh error: " message)
                                mouse-face mode-line-highlight)))))
           (t
            (with-current-buffer buffer
              (gosh-mode-put :modeline-load-status
                '(:propertize "Valid" face gosh-modeline-normal-face))))))))))

(defun gosh-test--parse-error-message (message)
  (save-excursion
    (cond
     ((string-match ":line \\([0-9]+\\):\\(.*\\)" message)
      (let ((line (string-to-number (match-string 1 message)))
            (text (match-string 2 message)))
        (gosh-goto-line line)
        ;; move backward until line is not empty.
        (while (and (not (bobp))
                    (= (point) (line-end-position)))
          (forward-line -1))
        (gosh-test--make-error
         (point) (line-end-position)
         text 'flymake-errline))))))

(defun gosh-test--clear-errors ()
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (flymake-overlay-p ov)
        (delete-overlay ov)))))

(defun gosh-test--process-sentinel (proc event)
  (unless (eq (process-status proc) 'run)
    (let ((buffer (process-get proc 'gosh-test-source-buffer))
          result errors)
      (if (not (buffer-live-p buffer))
          (gosh-test--close-process proc)
        (with-current-buffer buffer
          (setq result
                (unwind-protect
                    (with-current-buffer (process-buffer proc)
                      (catch 'done
                        (unless (= (process-exit-status proc) 0)
                          (throw 'done
                                 `(:propertize
                                   "NG"
                                   face gosh-modeline-error-face
                                   help-echo "Gosh test error: Unable to load this file"
                                   mouse-face mode-line-highlight)))
                        (goto-char (point-min))
                        (while (re-search-forward "ERROR:[\s\t]*\\(.*\\)" nil t)
                          (setq errors (cons (match-string 1) errors)))
                        (when errors
                          (throw 'done
                                 `(:propertize
                                   "NG"
                                   face gosh-modeline-error-face
                                   help-echo ,(concat "Gosh test error: "
                                                      (prin1-to-string errors))
                                   mouse-face mode-line-highlight)))
                        `(:propertize "OK"
                                      face gosh-modeline-normal-face)))
                  (gosh-test--close-process proc)))
          (gosh-mode-put :modeline-test-result result)
          (gosh-test--highilght-errors-if-possible errors))))))

(defun gosh-test--close-process (proc)
  (delete-process proc)
  (kill-buffer (process-buffer proc)))

(defun gosh-test--initialize ()
  (add-to-list 'gosh-mode-line-process
               '(t gosh-test--modeline-format))
  (gosh-test--reset)
  (add-hook 'gosh-mode-timer-functions
            'gosh-test--update-modeline nil t)
  (add-hook 'after-save-hook
            'gosh-test--update-modeline nil t))


;;;
;;; Refactor
;;;

(defvar gosh-refactor-read-symbol-history nil)

(defface gosh-refactor-on-cursor-face
  '((t (:inherit isearch)))
  "Face for highlighting of symbol at cursor."
  :group 'gosh-mode)

(defface gosh-refactor-scheduled-local-face
  '((t (:inherit lazy-highlight)))
  "Face for highlighting of matches that is current definition."
  :group 'gosh-mode)

(defface gosh-refactor-scheduled-global-face
  '((t (:bold t :inherit lazy-highlight)))
  "Face for highlighting of matches."
  :group 'gosh-mode)

(defface gosh-refactor-ignore-face
  '((t (:background "yellow" :inherit shadow)))
  "Face for highlighting of ignore while renaming."
  :group 'gosh-mode)

(defface gosh-refactor-replaced-face
  '((t (:inherit match)))
  "Face for highlighting of matches that was replaced successfully."
  :group 'gosh-mode)

(defface gosh-refactor-warning-face
  '((t (:inherit font-lock-warning-face)))
  "Face for highlighting of matches that is same as new definition."
  :group 'gosh-mode)

(defun gosh-refactor--goto-top-of-form (&optional point)
  (let ((parse (nth 9 (parse-partial-sexp
                       (point-min) (or point (point))))))
    (when parse
      (goto-char (car (last parse))))))

(defun gosh-refactor--goto-toplevel ()
  (let ((parse (nth 9 (parse-partial-sexp (point-min) (point)))))
    (when parse
      (goto-char (car parse)))))

(defun gosh-refactor-rename-symbol-read-args ()
  (barf-if-buffer-read-only)
  (let* ((sym (gosh-parse-symbol-at-point))
         (current (symbol-name sym))
         prompt new)
    (unless (and sym current)
      (error "No symbol at point"))
    (when (string-match "\\`\\^[a-z]\\'" current)
      (setq current (substring current 1)))
    (setq prompt (format "%s -> New name: " current))
    (setq new
          (read-string prompt current
                       'gosh-refactor-read-symbol-history))
    (when (string= new "")
      (error "No new symbol"))
    (when (string= current new)
      (error "No difference"))
    (list current new)))

(defun gosh-refactor-find-executable-scripts ()
  (let (list)
    (dolist (path exec-path)
      (dolist (file (directory-files path t "^[^.]"))
        (when (and (file-writable-p file)
                   (not (eq (car (file-attributes file)) t)))
          ;; read shebang
          (with-temp-buffer
            (let ((coding-system-for-read 'raw-text))
              (insert-file-contents file nil 0 256))
            (goto-char (point-min))
            (when (looking-at "#!.*/gosh\\(\\.exe\\)?$")
              (setq list (cons file list)))))))
    list))

(defun gosh-refactor-rename-symbol (old-name new-name)
  "Rename symbol at point."
  (interactive (gosh-refactor-rename-symbol-read-args))
  ;; clear highlight just in case
  (gosh-refactor--dehighlight)
  (unwind-protect
      (progn
        (gosh-refactor--highlight-symbol old-name)
        (save-excursion
          (let ((highlighter (make-overlay (point-min) (point-min))))
            (unwind-protect
                (progn
                  (overlay-put highlighter 'face 'highlight)
                  (gosh-refactor--interactive-replace new-name highlighter))
              (delete-overlay highlighter)))))
    (gosh-refactor--dehighlight)))

(defun gosh-refactor--query (prompt)
  ;; TODO `?' char popup help.
  (loop with msg = (format
                    "%s (Y)es, (N)o, (A)ll of highlighting, (Q)uit or All in buffer(!): "
                    prompt)
        with c
        while t
        do (setq c (downcase (read-char msg)))
        if (eq c ?\y)
        return 'yes
        else if (eq c ?n)
        return 'no
        else if (eq c ?q)
        return 'quit
        else if (eq c ?a)
        return 'all-of-highlight
        else if (eq c ?!)
        return 'all-of-buffer))

(defun gosh-refactor--overlays-in (start end)
  (gosh-filter
   (lambda (ov) (overlay-get ov 'gosh-refactor-overlay-p))
   (overlays-in start end)))

(defun gosh-refactor--scheduled-overlays (start end)
  (gosh-refactor--sort-overlays
   (gosh-remove
    (lambda (ov) (overlay-get ov 'gosh-refactor-done))
    (gosh-refactor--overlays-in start end))))

(defun gosh-refactor--sort-overlays (overlays)
  (sort overlays
        (lambda (o1 o2)
          (< (overlay-start o1) (overlay-start o2)))))

(defun gosh-refactor--next-region (point)
  (cond
   ((= (point-min) (point)) nil)
   ((gosh-context-toplevel-p point)
    (cons (point-min-marker) (point-max-marker)))
   (t
    (save-excursion
      (goto-char point)
      (let (start end)
        (gosh-refactor--goto-top-of-form)
        (setq start (point-marker))
        (gosh-read)
        (setq end (point-marker))
        (cons start end))))))

(defun gosh-refactor--check-bound-region (new text region &optional region-1)
  "Highlight NEW regexp as warning face."
  (let ((warns (gosh-refactor--highlight-bound-region new region region-1)))
    (when warns
      ;;TODO
      (unless (gosh-refactor--confirm-with-popup
               (format "New text `%s' is already bound. Continue? " text) warns)
        ;;TODO
        (signal 'quit nil)))))

(defun gosh-refactor--highlight-bound-region (new region &optional region-1)
  (save-excursion
    (goto-char (car region))
    (let ((case-fold-search)
          (ovs '()))
      (while (re-search-forward new (cdr region) t)
        (unless (and region-1
                     (< (car region-1) (point))
                     (< (point) (cdr region-1)))
          (let* ((match (reverse (match-data)))
                 ;; last subexp
                 (end (car match))
                 (start (cadr match)))
            (let ((ov (make-overlay start end)))
              (overlay-put ov 'priority 1000)
              (overlay-put ov 'face 'gosh-refactor-warning-face)
              (overlay-put ov 'gosh-refactor-bound-p t)
              (setq ovs (cons ov ovs))))))
      (nreverse ovs))))

(defun gosh-refactor--interactive-replace (new-string highlight)
  "Replace highlighted text interactively by NEW-STRING.
HIGHLIGHT is a marker to make be explicitly the target is."
  (loop with point = (point)
        with no-confirm
        with done
        with prev-region
        with new = (gosh-refactor--symbol-regexp new-string)
        do
        (let* ((region (gosh-refactor--next-region point))
               (start (car region))
               (end (cdr region))
               (ovs (gosh-refactor--scheduled-overlays start end)))
          (loop initially (progn
                            (gosh-refactor--check-bound-region
                             new new-string region prev-region)
                            (move-overlay highlight start end)
                            (unless (memq no-confirm '(all-of-buffer))
                              (setq no-confirm nil)))
                for ov in ovs
                do
                (let ((old-face (overlay-get ov 'face)))
                  (goto-char (overlay-start ov))
                  (overlay-put ov 'face 'gosh-refactor-on-cursor-face)
                  (unwind-protect
                      (let ((query (or no-confirm
                                       (gosh-refactor--query "Rename: "))))
                        (case query
                          ('yes
                           (gosh-refactor--replace-string ov new-string))
                          ('no
                           (gosh-refactor--mark-as-finished ov))
                          ('quit
                           (setq done t)
                           (return))
                          ('all-of-buffer
                           (gosh-refactor--replace-string ov new-string)
                           (sit-for 0.2)
                           (setq start (save-excursion
                                         (gosh-refactor--goto-toplevel)
                                         (point)))
                           (setq no-confirm query))
                          ('all-of-highlight
                           (gosh-refactor--replace-string ov new-string)
                           (sit-for 0.2)
                           (setq no-confirm query))))
                    (when (eq (overlay-get ov 'face) 'gosh-refactor-on-cursor-face)
                      (overlay-put ov 'face old-face)))))
          (setq prev-region region)
          ;; move base point to start of region
          (setq point start))
        ;;TODO consider this condition
        never (or done (= point (point-min)))))

(defun gosh-refactor--dehighlight ()
  (remove-overlays (point-min) (point-max) 'gosh-refactor-overlay-p t)
  (remove-overlays (point-min) (point-max) 'gosh-refactor-bound-p t))

(defun gosh-refactor--highlight-symbol (symbol)
  (let ((regexp (gosh-refactor--symbol-regexp symbol)))
    (gosh-refactor--highlight-current-define regexp)
    (gosh-refactor--highlight-buffer regexp)))

(defun gosh-refactor--highlight-buffer (regexp)
  (save-excursion
    (save-restriction
      (widen)
      (gosh-refactor--highlight-region
       (point-min) (point-max) regexp
       'gosh-refactor-scheduled-global-face))))

(defun gosh-refactor--highlight-current-define (regexp)
  (let (start end)
    (save-excursion
      (unless (re-search-backward "^(" nil t)
        (error "Invalid sexp"))
      (setq start (point-marker))
      (gosh-read)
      (setq end (point-marker)))
    (gosh-refactor--highlight-region
     start end regexp
     'gosh-refactor-scheduled-local-face)))

(defun gosh-refactor--highlight-region (start end regexp face)
  "highlight START to END word that match to REGEXP.
CHECK is function that accept no arg and return boolean."
  (save-match-data
    (save-excursion
      (goto-char start)
      (let ((case-fold-search)
            (ovs '()))
        (while (and (re-search-forward regexp nil t)
                    (< (point) end))
          (let* ((match (reverse (match-data)))
                 ;; last subexp
                 (end (car match))
                 (start (cadr match)))
            (unless (gosh-refactor--overlays-in start end)
              (let((ov (make-overlay start end)))
                (overlay-put ov 'priority 1000)
                (overlay-put ov 'face face)
                (overlay-put ov 'gosh-refactor-overlay-p t)
                (setq ovs (cons ov ovs))))))
        (nreverse ovs)))))

(defun gosh-refactor--mark-as-finished (ov)
  (overlay-put ov 'face 'gosh-refactor-ignore-face)
  (overlay-put ov 'gosh-refactor-done t))

(defun gosh-refactor--replace-string (ov new-string)
  (let* ((start (overlay-start ov))
         (end (overlay-end ov))
         (old (buffer-substring start end)))
    (overlay-put ov 'gosh-refactor-old-text old)
    (overlay-put ov 'gosh-refactor-done t)
    (overlay-put ov 'face 'gosh-refactor-replaced-face)
    (goto-char start)
    (delete-region start end)
    (setq start (point))
    (insert-before-markers new-string)
    (setq end (point))
    ;; restore overlay visible
    (move-overlay ov start end)))

(defun gosh-refactor--symbol-regexp (symbol)
  (cond
   ;; consider the ^a ^b like lambda generator
   ((string-match "\\`\\^?\\([a-z]\\)\\'" symbol)
    (let ((name (match-string 1 symbol)))
      (format "\\_<\\(?:\\^\\(%s\\)\\|\\(%s\\)\\)\\_>" name name)))
   (t
    (format "\\_<\\(%s\\)\\_>" (regexp-quote symbol)))))

(defun gosh-refactor--confirm-with-popup (prompt overlays)
  (save-window-excursion
    (save-excursion
      (gosh-refactor--rotate-overlays (car overlays) overlays)
      (unwind-protect
          (y-or-n-p prompt)
        (when gosh-refactor--rotate-timer
          (cancel-timer gosh-refactor--rotate-timer)
          (setq gosh-refactor--rotate-timer nil))))))

(defvar gosh-refactor--rotate-timer nil)
(defun gosh-refactor--rotate-overlays (ov overlays)
  (when overlays
    (save-match-data
      (with-local-quit
        (switch-to-buffer (overlay-buffer ov))
        (goto-char (overlay-start ov))
        (recenter)
        (let ((next (gosh-refactor--rotate-next-overlay ov overlays)))
          (setq gosh-refactor--rotate-timer
                (run-with-timer
                 1 nil
                 'gosh-refactor--rotate-overlays
                 (or next (car overlays)) overlays)))))))

(defun gosh-refactor--rotate-next-overlay (base overlays)
  (let ((first (line-number-at-pos (window-start)))
        (last (line-number-at-pos (window-end))))
    (loop for next on (cdr (memq base overlays))
          if (>= (line-number-at-pos (overlay-start (car next))) last)
          return (car next))))

;; ;;TODO
;; ;; *load-path* files
;; ;; writable PATH files?
;; ;; name -> dwim?
;; ;; this symbol is not defined in this module.
;; ;;  jump to the module and try again.
;; (defun gosh-refactor-rename-symbol-afaiui (old-name new-name)
;;   "Rename symbol As Far As I Understand It"
;;   ;; (split-string (getenv "GAUCHE_LOAD_PATH") path-separator)
;;   ;; (scheme-current-globals)
;;   )


;;;;
;;;; Mode definitions
;;;;

;;;
;;; gosh-mode
;;;

(defvar gosh-mode-map nil)

(unless gosh-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scheme-mode-map)

    (define-key map "\M-\C-i" 'gosh-smart-complete)
    (define-key map "\C-c\M-I" 'gosh-import-module-thingatpt)
    (define-key map "\C-c\C-j" 'gosh-jump-thingatpt)
    (define-key map "\C-c\C-u" 'gosh-test-module)
    (define-key map "\C-c?" 'gosh-show-info)
    (define-key map "\C-c\M-r" 'gosh-refactor-rename-symbol)
    ;; (define-key map "\C-c\er" 'gosh-refactor-rename-symbol-afaiui)
    (define-key map "\C-c\C-p" 'gosh-test-popup-result)

    (define-key map ")" 'gosh-closing-insert-paren)
    (define-key map "]" 'gosh-closing-insert-bracket)
    (define-key map "(" 'gosh-opening-insert-paren)
    (define-key map "[" 'gosh-opening-insert-bracket)

    (setq gosh-mode-map map)))

(defvar gosh-mode-hook nil)

(defvar gosh-mode--timer nil
  "Watcher process for `gosh-mode'")

(defvar gosh-mode--timer-delay 1
  "Idle delay seconds of `gosh-mode--timer'.")

(defvar gosh-mode-line-process nil)
(make-variable-buffer-local 'gosh-mode-line-process)
(put 'gosh-mode-line-process 'risky-local-variable t)

(defvar gosh-mode-timer-functions nil)

(defvar gosh-mode--context nil)
(make-variable-buffer-local 'gosh-mode--context)
(put 'gosh-mode--context 'risky-local-variable t)

(defvar gosh-mode-abbrev-table nil)
(define-abbrev-table 'gosh-mode-abbrev-table ())

;;;###autoload
(define-derived-mode gosh-mode scheme-mode "Gosh"
  "Major mode for Gauche programming."
  (if (boundp 'font-lock-syntactic-keywords)
      (set (make-local-variable 'font-lock-syntactic-keywords)
           (append
            font-lock-syntactic-keywords
            gosh-font-lock-syntactic-keywords))
    ;; FIXME:
    ;;  must change to use `syntax-propertize-function' after 24.1
    (set (make-local-variable 'syntax-propertize-function)
         'gosh-syntax-table-apply-region))
  (set (make-local-variable 'after-change-functions)
       'gosh-after-change-function)
  ;;TODO cancel-timer after kill all gosh-mode
  (unless gosh-mode--timer
    (setq gosh-mode--timer
          (run-with-idle-timer gosh-mode--timer-delay t
                               'gosh-mode-watcher)))
  (add-to-list (make-local-variable 'mode-line-process)
               'gosh-mode-line-process
               'append)
  (gosh-font-lock--initialize)
  (gosh-mode-put :modtime (float-time))
  (gosh-eldoc--initialize)
  (gosh-ac--initialize-for-mode)
  (gosh-test--initialize)
  (use-local-map gosh-mode-map)
  (run-mode-hooks 'gosh-mode-hook))

(defun gosh-mode-watcher ()
  ;; check buffer mode to avoid endless freeze
  (save-match-data
    (with-local-quit
      (when (and (eq major-mode 'gosh-mode)
                 (not (minibufferp (current-buffer))))
        (run-hooks 'gosh-mode-timer-functions)))))

(defun gosh-mode-get (key)
  (plist-get gosh-mode--context key))

(defmacro gosh-mode-put (key value)
  (declare (indent 1))
  (let ((val (make-symbol "val")))
    `(let ((,val ,value))
       (if gosh-mode--context
           (plist-put gosh-mode--context ,key ,val)
         (setq gosh-mode--context (list ,key ,val)))
       ,val)))

(defun gosh-after-change-function (start end old-len)
  ;;check executable
  (when (< end 255)
    (gosh-mode-put :executable nil))
  (gosh-mode-put :modtime (float-time))
  (gosh-test--reset-status))

(defun gosh-mode--temp-file ()
  (let (file)
    (if (and buffer-file-name
             (not (buffer-modified-p (current-buffer)))
             ;; in archive file
             (file-exists-p buffer-file-name))
        (setq file buffer-file-name)
      (unless (gosh-mode-get :temp-file)
        (gosh-mode-put :temp-file
          (make-temp-file "gosh-mode-")))
      (setq file (gosh-mode-get :temp-file))
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region (point-min) (point-max) file nil 'no-msg)))
    file))

;;
;; font-lock
;;

(defun gosh-font-lock--initialize ()
  ;; clone font lock settings with preserving scheme
  ;; original settings.
  (setq font-lock-defaults
        (gosh-font-lock--clone-keywords font-lock-defaults))
  (setcar font-lock-defaults
          '(gosh-font-lock--keywords
            gosh-font-lock--keywords-2)))

;; font lock user customizable
(font-lock-add-keywords
 'gosh-mode
 `(("\\`#.+" 0 font-lock-comment-delimiter-face)
   (gosh-font-lock-procedure-keywords 1 font-lock-keyword-face)
   (gosh-font-lock-syntax-keywords 1 font-lock-constant-face)
   (gosh-font-lock-basic-syntax
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face nil t))))

(defun gosh-font-lock-procedure-keywords (bound)
  ;; ignore if quack is activated
  (and (not (featurep 'quack))
       (re-search-forward gosh-defined-procedure-keyword-regexp bound t)))

(defun gosh-font-lock-syntax-keywords (bound)
  (and (not (featurep 'quack))
       (re-search-forward gosh-defined-generic-keyword-regexp bound t)))

(defun gosh-font-lock-basic-syntax (bound)
  ;; ignore if quack is activated
  (and (not (featurep 'quack))
       (re-search-forward gosh-basic-syntax-keyword-regexp bound t)))

(defun gosh-font-lock--clone-keywords (keywords)
  (let ((ks keywords)
        res)
    (while (consp ks)
      (setq res
            (append
             res
             (list
              (if (consp (car ks))
                  (gosh-font-lock--clone-keywords (car ks))
                (car ks)))))
      (setq ks (cdr ks)))
    (when ks
      (setq res (append res ks)))
    res))

;; modify scheme fontify rule
;; gauche accept `[' same as `('
(defun gosh-font-lock--modify-scheme-keywords (keywords)
  (dolist (key keywords)
    (let ((regexp (car key)))
      (when (and (stringp regexp) (string-match "^(" regexp))
        (setcar key (concat "[[(]" (substring regexp 1)))))))

(defvar gosh-font-lock--keywords-2
  (let ((keywords (gosh-font-lock--clone-keywords scheme-font-lock-keywords-2)))
    (setq keywords
          (cons
           `(
             ,(concat
               "(\\(define\\*?\\(?:"
               "\\(-constant\\)"
               "\\)\\)\\_>[\s\t]*(?\\([^\s\t\n]+\\)"
               )
             (1 font-lock-keyword-face)
             (3 (cond
                 ((match-beginning 2) font-lock-variable-name-face)
                 (t font-lock-type-face)) nil t))
           keywords))
    (gosh-font-lock--modify-scheme-keywords keywords)
    keywords))

(defvar gosh-font-lock--keywords
  (let ((keywords (gosh-font-lock--clone-keywords scheme-font-lock-keywords)))
    (gosh-font-lock--modify-scheme-keywords keywords)
    keywords))

;;
;; syntax
;;

(defconst gosh-font-lock-syntactic-keywords
  `(
    (,gosh-regexp-literal-regexp
     ;; (15) is generic string delimiter
     (1 (6) t) (2 (15)) (4 (15) nil t) (5 (15) nil t))
    ))

(defun gosh-syntax-table-apply-region (start end)
  (let ((modified (buffer-modified-p)))
    (unwind-protect
        (save-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (let ((inhibit-read-only t))
              (while (re-search-forward "#/" nil t)
                (let ((beg (match-beginning 0)))
                  (when (gosh-context-code-p beg)
                    (gosh-syntax-table-set-properties beg)))))))
      (set-buffer-modified-p modified))))

(defun gosh-syntax-table-put-property (beg end value)
  (put-text-property beg end 'syntax-table value (current-buffer)))

(defun gosh-syntax-table-set-properties (beg)
  (let ((curpos beg)
        ;; "aa #/regexp/"
        ;; #/ignore case regexp/i
        ;; #/regexp \/contains slash \//
        (max (min (line-end-position 5) (point-max)))
        (state 0))
    (while (and (< curpos max)
                (< state 3))
      (cond
       ((= state 0)
        (when (= (char-after curpos) ?#)
          ;; (6) = expression prefix
          (gosh-syntax-table-put-property curpos (1+ curpos) '(6)))
        (setq state (+ 1 state)))

       ((= state 1)
        (when (= (char-after curpos) ?/)
          ;; (15) = generic string delimiter
          (gosh-syntax-table-put-property curpos (1+ curpos) '(15)))
        (setq state (+ 1 state)))

       ((= state 2)
        (cond
         ((= (char-after curpos) ?\\)
          ;; (9) = escape
          (gosh-syntax-table-put-property curpos (1+ curpos) '(9))
          (setq curpos (1+ curpos)))
         ;; handle backslash inside the string
         ((= (char-after curpos) ?/)
          (cond
           ((= (char-after (1+ curpos)) ?i)
            (setq curpos (1+ curpos))
            (gosh-syntax-table-put-property curpos (1+ curpos) '(15))
            (setq state (1+ state)))
           (t
            ;; finish regexp literal
            (gosh-syntax-table-put-property curpos (1+ curpos) '(15))
            (setq state (1+ state)))))

         ;; everything else
         (t
          nil))))
      ;; next char
      (setq curpos (+ curpos 1)))))

;;
;; command
;;

(defun gosh-jump-to-module-def (module symbol)
  (let* ((mf (or (and (stringp module) module)
                 (gosh--module->file module)))
         (buf (get-file-buffer mf)))
    (set-buffer (or buf (gosh--find-file-noselect mf)))
    (when (gosh-jump-to-def symbol)
      (switch-to-buffer (current-buffer))
      t)))

(defun gosh-jump-to-def (definition)
  (let* ((name (symbol-name definition))
         (first (point))
         (regexp (format "^[\s\t]*(def\\(?:\\s_\\|\\sw\\)*\\(?:(\\|[\s\t]\\)+\\_<%s\\_>"
                         (regexp-quote name))))
    (goto-char (point-min))
    (cond
     ((re-search-forward regexp nil t)
      (push-mark first)
      (forward-line 0))
     (t
      (goto-char first)
      nil))))

(defun gosh--insert-import-statement (module)
  ;;FIXME
  (let ((inhibit-read-only t))
    (insert (format "(use %s)\n" module))
    (indent-for-tab-command)
    (message "Module %s is imported now." module)
    (sit-for 0.5)))

(defun gosh-sort-sexp-region (start end)
  "Sort sexp between START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((lis nil)
            (reg-start (point))
            reg-end)
        (condition-case nil
            (while (not (eobp))
              (let* ((beg (point))
                     (sexp (gosh-read))
                     (end (point))
                     (key (prin1-to-string sexp))
                     (contents (buffer-substring beg end)))
                (setq reg-end end)
                (setq lis (cons (list key contents) lis))))
          (error nil))
        (when reg-end
          (setq lis (sort lis (lambda (x y) (string-lessp (car x) (car y)))))
          (delete-region reg-start reg-end)
          (goto-char reg-start)
          (dolist (s lis)
            (let ((contents (cadr s)))
              (when (string-match "\\`[\s\t\n]+" contents)
                (setq contents (substring contents (match-end 0))))
              (insert contents)
              (unless (memq (char-before) '(?\n))
                (insert "\n")))))))))

(defun gosh-jump-thingatpt ()
  "Jump to current symbol definition.
"
  (interactive)
  (let* ((sym (gosh-parse-symbol-at-point)))
    (catch 'found
      ;; search local variable
      (when (assq sym (gosh-parse--current-local-vars))
        (let* ((first (point))
               (start (or (prog1
                              (re-search-backward "^(" nil t)
                            (forward-char 1))
                          (point-min)))
               (end (or (re-search-forward "^(" nil t) (point-max))))
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (or
             (and (re-search-forward (format "\\_<%s\\_>" sym) nil t)
                  (throw 'found t))
             (and (gosh-jump-to-def sym)
                  (throw 'found t))))
          ;; jump is faild.
          (goto-char first)))
      ;; search toplevel definitions
      (when (assq sym (gosh-parse--current-globals t))
        (gosh-jump-to-def sym)
        (throw 'found t))
      ;; search base modules
      (dolist (x (ignore-errors (gosh-parse--current-base-modules)))
        (when (gosh-jump-to-module-def x sym)
          (throw 'found t)))
      ;; search imported modules
      (let ((modules (gosh-parse-gather-current-importer)))
        (dolist (m (gosh-guessed-module-files modules (symbol-name sym)))
          (let ((mf (or (and (stringp m) m) (car m)))
                (sym2 (or (and (listp m)
                               (intern-soft (substring (symbol-name sym) (length (cdr m)))))
                          sym)))
            (when (memq sym2 (gosh-parse-exports-functions mf))
              (when (gosh-jump-to-module-def mf sym2)
                (throw 'found t))))))
      ;; jump to module (cursor point as a module name)
      (let ((file (gosh--module->file sym)))
        (when file
          (switch-to-buffer (gosh--find-file-noselect file))
          (throw 'found t)))
      (message "Not found definition %s" sym))))

(defun gosh-import-module-thingatpt ()
  "Import a new module if missing.

if define-module is exists then insert statement to the form of `define-module'
otherwise insert top level of the script."
  (interactive)
  (barf-if-buffer-read-only)
  (let* ((sym (gosh-parse-symbol-at-point))
         (cm (gosh-parse-current-module))
         module)
    (catch 'found
      (unless sym (throw 'found 'not-found))
      ;; search imported modules
      (setq module (gosh-info-doc-guess-module sym))
      (when module
        (throw 'found t))
      (message "Exported module is not found `%s'" sym))
    (unless module
      (error "Module for `%s' is not found" sym))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward (format "(use[\s\t\n]+\\_<%s\\_>" module) nil t)
        (cond
         ((gosh-context-code-p)
          (ding)
          (message "Module %s have already imported." module))
         ((and (forward-line 0)
               (looking-at "^\\([\s\t]*;+[\s\t]*\\)"))
          ;; uncomment guessed as temporarily commented out statement.
          (replace-match "" nil nil nil 1))))
       ((re-search-forward "^ *\\((use\\_>\\)" nil t) ; first `use' statements
        (goto-char (match-beginning 1))
        (gosh--insert-import-statement module))
       ((let ((regexp (format "^ *(define-module[\s\t]+%s[\s\t\n]" (regexp-quote cm))))
          (re-search-forward regexp nil t))
        (forward-line 1)
        (gosh--insert-import-statement module))
       ((re-search-forward "^ *(" nil t) ; first statement
        (forward-line 0)
        (gosh--insert-import-statement module))
       (t
        (error "Unable find properly point to insert import statement"))))))

(defun gosh-show-info (symbol-name)
  "Popup `info' buffer"
  (interactive
   (let ((sym (gosh-parse-symbol-at-point)))
     (list (symbol-name sym))))
  (let (message-log-max)
    (info-lookup-symbol symbol-name)))


;;;
;;; Sticky minor mode
;;;

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
Evaluate s-expression, syntax check, etc."
  nil nil gosh-sticky-mode-map
  (if gosh-sticky-mode
      (add-hook 'gosh-mode-timer-functions 'gosh-sticky-backend-watcher nil t)
    (remove-hook 'gosh-mode-timer-functions 'gosh-sticky-backend-watcher t))
  (gosh-sticky-backend-switch-context))

(defun gosh-sticky-mode-on ()
  (gosh-sticky-mode 1))

(defun gosh-sticky-mode-off ()
  (gosh-sticky-mode -1))

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

(defvar gosh-backend-prompt-string-regexp "\ngosh>[\s\t]*$")
(defvar gosh-backend-prompt-regexp "^gosh>[\s\t]*\\'")
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
    (error "Assert"))
  ;;TODO switch by executable
  (let* ((command gosh-default-command-internal)
         (proc (gosh-backend-active-process command)))
    (unless proc
      (let* ((buffer (gosh-backend-process-buffer command))
             (dir default-directory))
        (with-current-buffer buffer
          (unless (file-directory-p default-directory)
            (cd dir))
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

;; TODO detect changing GAUCHE_LOAD_PATH, GAUCHE_DYNLOAD_PATH??
;;    but no way to remove from *load-path*
;; http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3aWishList&l=ja#H-1mx5tqr92ymmz
;; TODO detect changing environment-variable
;;    sys-putenv
;;    evaluate sexp contains sys-putenv...
;;    no sys-unsetenv ...
(defun gosh-sticky-backend-switch-context ()
  (let* ((proc (gosh-backend-check-process))
         (file (gosh-mode--temp-file))
         (directory (expand-file-name default-directory)))
    (unless (file-directory-p directory)
      (error "Directory is not exist"))
    ;; filter is `gosh-sticky-switch-context-filter' means now switching context
    (when (and (not (eq (process-filter proc) 'gosh-sticky-switch-context-filter))
               (or (not (eq (process-get proc 'gosh-backend-current-buffer)
                            (current-buffer)))
                   (null (process-get proc 'gosh-backend-switched-time))
                   (null (gosh-mode-get :modtime))
                   (>= (gosh-mode-get :modtime)
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

(defvar gosh-read-expression-history nil)

(defun gosh-eval-expression-1 (eval-expression-arg &optional suppress-message)
  (let ((module (gosh-parse-current-module))
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
         (module (gosh-parse-current-module))
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

(defun gosh-sticky-backend-watcher ()
  (unless (gosh-backend-active-process)
    (gosh-sticky-backend-switch-context)))

(defun gosh-send-last-sexp ()
  "Send the previous sexp to the sticky backend process.
That sexp evaluated at current module. The module may not be loaded.
Execute \\[gosh-eval-buffer] if you certain the buffer is a reliable code.
"
  (interactive)
  (gosh-eval--check-backend)
  (if (gosh-parse-last-expression-define-p)
      (gosh-eval--last-toplevel-expression)
    (gosh-eval--send-region (save-excursion (backward-sexp) (point)) (point))))

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
    (let ((file (gosh-mode--temp-file)))
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


;;;
;;; inferior mode
;;;

(defvar gosh-inferior-mode-map nil)

(unless gosh-inferior-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\M-\C-i" 'gosh-smart-complete)
    (define-key map "\C-c?" 'gosh-show-info)
    (define-key map "\C-i" 'gosh-inferior-smart-complete)

    (setq gosh-inferior-mode-map map)))

(defvar gosh-inferior-mode-hook nil)

(defun gosh-inferior-mode ()
  "Interactive buffer have `auto-complete' feature."
  ;; DO NOT derived `inferior-scheme-mode'.
  ;; Because `run-scheme' execute `inferior-scheme-mode-hook'
  ;; derived major mode run the hook one more time.
  (gosh-eldoc--initialize)
  (gosh-ac--initialize-for-mode)
  (when (featurep 'auto-complete)
    (add-to-list 'ac-sources 'ac-source-gosh-inferior-symbols))
  (setq major-mode 'gosh-inferior-mode)
  (setq mode-name "Inferier Gosh")
  ;; clear
  (setq gosh-inferior--autoload-functions nil)
  (set-keymap-parent gosh-inferior-mode-map inferior-scheme-mode-map)
  (use-local-map gosh-inferior-mode-map)
  (run-mode-hooks 'gosh-inferior-mode-hook))

(defun gosh-inferior-process ()
  (when (boundp 'scheme-buffer)
    (let ((proc (get-buffer-process scheme-buffer)))
      (and proc (eq (process-status proc) 'run) proc))))

(defun gosh-inferior-symbol-candidates ()
  (let ((proc (gosh-inferior-process)))
    (unless gosh-inferior--autoload-functions
      (setq gosh-inferior--autoload-functions
            (gosh-inferior--autoload-functions proc)))
    (append
     gosh-inferior--autoload-functions
     (gosh-inferier--import-candidates proc))))

(defun gosh-inferier--import-candidates (proc)
  "Gather symbols from PROC current-module context"
  (gosh-snatch-process-candidates proc gosh-module-imports-command-string))

(defvar gosh-inferior--autoload-functions nil)
(defun gosh-inferior--autoload-functions (proc)
  "Gather autoload symbols from PROC"
  (gosh-snatch-process-candidates proc (gosh-autoload-symbols-command-string)))

(defun gosh-inferior-smart-complete ()
  "Smart complete in `gosh-inferior-mode'.
That candidates are gathered from current-module.
TODO but not supported with-module context."
  (interactive)
  (let ((cands (gosh-inferior-symbol-candidates))
        (sym (gosh-complete-symbol-name-at-point)))
    (gosh-completion-do sym cands)))



(defvar gosh-snatch--filter-candidates nil)
(defvar gosh-snatch--filter-string-stack nil)

(defun gosh-snatch-process-candidates (proc command)
  "Execute COMMAND in PROC"
  (let ((filter (process-filter proc)))
    (unless (eq filter 'gosh-snatch-process-filter)
      (set-process-filter proc 'gosh-snatch-process-filter)
      (unwind-protect
          (progn
            (setq gosh-snatch--filter-candidates nil
                  gosh-snatch--filter-string-stack nil)
            (process-send-string proc command)
            (let ((inhibit-quit t))
              (while (not gosh-snatch--filter-candidates)
                (sleep-for 0.1)
                (when quit-flag
                  (setq inhibit-quit nil)))))
        (set-process-filter proc filter))
      (and (listp gosh-snatch--filter-candidates)
           gosh-snatch--filter-candidates))))

;; FIXME Unable read scheme symbol `t'.
(defun gosh-snatch-process-filter (proc event)
  (condition-case err
      (progn
        (setq gosh-snatch--filter-string-stack
              (concat gosh-snatch--filter-string-stack event))
        (when (string-match gosh-backend-prompt-string-regexp gosh-snatch--filter-string-stack)
          ;; read scheme '() to nil but to finish up the filter change to `t'
          (setq gosh-snatch--filter-candidates
                (or
                 (read gosh-snatch--filter-string-stack)
                 t))))
    (error
     ;; step through if error.
     (setq gosh-snatch--filter-candidates t))))



(defun gosh-mode-unload-function ()
  ;;TODO
  )



(provide 'gosh-mode)

;;; gosh-mode.el ends here
