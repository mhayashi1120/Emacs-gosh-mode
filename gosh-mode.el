;;; gosh-mode.el --- Programming language gauche editing tools.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: lisp gauche scheme edit
;; URL: https://github.com/mhayashi1120/Emacs-gosh-mode
;; Emacs: GNU Emacs 28 or later
;; Version: 0.3.3
;; Package-Requires: ((emacs "28.1"))

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

;; ## Install:

;;TODO

;; ## Usage:

;; * `C-c C-u`

;; ## Thanks:
;; - scheme-complete.el
;;    gosh-mode is forked from scheme-complete.el
;;    Many code is duplicated but specialize to Gauche.

;;; Code:

(defgroup gosh-mode nil
  "Gauche script editing mode."
  :group 'lisp
  :prefix "gosh-")

(defvar gosh-mode-version "0.3.2")

(eval-when-compile
  (require 'gosh-const))

(require 'cl-lib)
(require 'gosh-const)
(require 'scheme)
(require 'cmuscheme)
(require 'info-look)
(require 'pcase)

(defvar path-separator)
(defvar process-environment)
(defvar current-prefix-arg)
(defvar inhibit-quit)
(defvar quit-flag)
(defvar unread-command-events)
(defvar exec-path)
(defvar auto-mode-interpreter-regexp)
(defvar read-expression-map)

;;TODO move
(defvar gosh-debug nil)


;;;;
;;;; utilities
;;;;

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
  (and (gosh-symbol-p symbol1)
       (gosh-symbol-p symbol2)
       (let ((name1 (gosh-symbol-name symbol1))
             (name2 (gosh-symbol-name symbol2)))
         (equal name1 name2))))

(defun gosh-symbol-name (symbol)
  (or
   (and symbol (symbolp symbol) (symbol-name symbol))
   (and (gosh-object-p symbol)
        (eq (gosh-object-type symbol) 'symbol)
        (gosh-object-value symbol))))

;;TODO consider change symbolp -> gosh-symbol-p
(defun gosh-symbol-p (obj)
  (or (and obj (symbolp obj))
      (and (gosh-object-p obj)
           (eq (gosh-object-type obj) 'symbol))))

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

(defun gosh--scan-sexps (point count)
  (condition-case nil
      (scan-sexps point count)
    (scan-error nil)))

(defun gosh-paren-against-char (char)
  (cl-case char
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

(defun gosh-filter (pred list)
  (cl-loop for l in list
           if (funcall pred l)
           collect l))

(defun gosh-remove (pred list)
  (cl-loop for l in list
           unless (funcall pred l)
           collect l))

(defun gosh-find (pred list)
  (cl-loop for l in list
           if (funcall pred l)
           return l))

(defun gosh-intersection (list1 list2)
  (cl-loop for l in list1
           if (member l list2)
           collect l))

(defun gosh-union (list1 list2)
  (cl-loop with res = list1
           for l in list2
           unless (member l res)
           do (setq res (cons l res))
           finally return res))

(defun gosh-append-map (proc init-ls)
  (let* ((ls (reverse init-ls))
         (res '()))
    (while (consp ls)
      (setq res (append (funcall proc (pop ls)) res)))
    res))

(defun gosh-append-map* (proc ls)
  (let ((res '()))
    (while (consp ls)
      (setq res (nconc (reverse (funcall proc (pop ls))) res)))
    (when ls
      (setq res (nconc (reverse (funcall proc ls)) res)))
    (nreverse res)))

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
    (setq n (1- n)
          ls (cdr ls)))
  (and (consp ls) (car ls)))

(defun gosh-length* (ls)
  (let ((n 0))
    (while (consp ls)
      (setq n (1+ n)
            ls (cdr ls)))
    n))

;;
;; string utilities
;;

(defun gosh-string-split-word (string)
  (cl-loop with start = 0
           while (string-match "\\w+" string start)
           collect (progn
                     (setq start (match-end 0))
                     (match-string 0 string))))

(defun gosh-string-prefix-p (pref str)
  (let ((p-len (length pref))
        (s-len (length str)))
    (and (<= p-len s-len)
         (equal pref (substring str 0 p-len)))))

;;
;; file utilities
;;

(defun gosh-any-file-in-path (file path)
  (catch 'found
    (dolist (dir path)
      (when (file-exists-p (concat dir "/" file))
        (throw 'found dir)))))

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

;;TODO consider userlock
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
               (when (string= (buffer-file-name ,buf) ,path)
                 (kill-buffer (current-buffer)))))
           ,res)))))

(defun gosh-file-mtime (file)
  (float-time (nth 5 (file-attributes file))))


;;;
;;; Gauche sexp <-> Emacs sexp
;;;

;; list -> list
;; number -> number or gosh-object[number]
;; vector -> gosh-object [vector]
;; string -> string
;; interpolate-string -> string
;; regexp -> gosh-object [regexp]
;; iregexp -> gosh-object [iregexp]

(defun gosh-object-p (x)
  (and (vectorp x) (= (length x) 2)))

(defun gosh-object-type (obj)
  (aref obj 0))

(defun gosh-object-value (obj)
  (aref obj 1))

(defun gosh-object (type value)
  (unless (memq type
                '(
                  char charset number vector regexp iregexp

                  u8vector u16vector u32vector u64vector
                  s8vector s16vector s32vector s64vector
                  f16vector f32vector f64vector

                  ;; only special symbol `nil'
                  symbol

                  unquote uninterned-symbol reader-constructor
                  back-reference
                  ))
    (error "Not a supported type %s" type))
  (vector type value))


;;;
;;; Gauche multiple values -> Emacs sexp
;;;

(defun gosh-multivalues-p (x)
  ;;TODO .....
  (and (vectorp x)
       (= (length x) 3)
       (eq (aref x 0) 'multivalues)))

;; TODO out of index
(defun gosh-multivalues-ref (mv n)
  (nth n (aref mv 2)))

(defun gosh-multivalues (list)
  ;;TODO aref 1 orz
  (vector 'multivalues nil list))


;;;
;;; Gauche pseudo reader
;;;

;; TODO reconsider all parsing code use this (Not exactly correct only "\s\t\n")
(defconst gosh-reader-ws "\t\n\v\f\r\s")

;;TODO escaped char
;; "A\ B" captured as is but must be "A B"
;; read.c ctypes[]
(defconst gosh-reader--word-re
  (let ((chars "][\000-\037\s\"'(),;\\`{|}\177"))
    (format "\\(\\(?:\\\\.\\|[^%s]\\)+\\)"
            chars)))

;; TODO check regexp
(defconst gosh-reader--charset-re
  "\\[\\(\\(?:\\[[^\[]+?\\]\\|\\\\.\\|[^\]]\\)*?\\)\\]")

;; TODO check regexp
(defconst gosh-reader--string-re
  "\"\\(\\(?:\\\\\.\\|[^\"]\\)*\\)\"")

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
     (t
      (when gosh-reader--fold-case
        (setq text (downcase text)))
      ;; To avoid misunderstand as empty list '()
      (if (string= text "nil")
          (gosh-object 'symbol "nil")
        (intern text))))))

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
  (skip-chars-forward gosh-reader-ws))

(defun gosh-reader--skip-nested-comments ()
  (let ((nested 1)                      ; already read first `#|'
        (start (point)))
    (while (> nested 0)
      (cond
       ((eobp)
        (signal 'invalid-read-syntax '("Unfinished nested comment")))
       ((looking-at "#|")
        (setq nested (1+ nested))
        (forward-char 2))
       ((looking-at "|#")
        (setq nested (1- nested))
        (forward-char 2))
       (t
        (forward-char)
        ;; Only `forward-char' may slow down the reader.
        ;; skip to at least meaningful char and go back.
        (or (and (re-search-forward "[#|]" nil t)
                 (progn (forward-char -1) t))
            (goto-char (point-max))))))
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
    (signal 'invalid-read-syntax (list "Nothing to consume")))
  (goto-char (match-end 0))
  (length (match-string 0)))

(defun gosh-reader--read-by-regexp (regexp subexp &optional no-error)
  (cond
   ((looking-at regexp)
    (goto-char (match-end 0))
    (match-string-no-properties subexp))
   (no-error nil)
   (t
    (signal 'invalid-read-syntax (list "Not a valid context")))))

(defun gosh-reader--read-hex (length)
  (let ((match (or
                (gosh-reader--read-by-regexp
                 ;; R7RS new syntax
                 "\\([a-fA-F0-9]+\\);" 1 t)
                (gosh-reader--read-by-regexp
                 (format "[a-fA-F0-9]\\{%s\\}" length) 0))))
    (condition-case nil
        (string-to-number match 16)
      ;; FIXME: ignore overflow
      (error 0))))

(defun gosh-reader--read-ucs (hex-length)
  (let ((n (gosh-reader--read-hex hex-length)))
    (encode-char n 'ucs)))

(defun gosh-reader--read-special-string ()
  (forward-char)
  (unless (eq (char-after) ?\")
    (signal 'invalid-read-syntax (list "Not a valid special string")))
  (gosh-reader--read-string))

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

    ("alarm"     . ?\x07)
    ("backspace" . ?\x08)
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
  (unless (looking-at "\\([usUS]\\(?:8\\|16\\|32\\|64\\)\\|[fF]\\(?:16\\|32\\|64\\)\\)(")
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
  (let ((start (point))
        (regexp (format "\\(\\(?:\\\\.\\|[^/]\\)*/\\)\\(i\\)?")))
    (forward-char)
    (unless (looking-at regexp)
      (signal 'invalid-read-syntax
              (list (format "Not a valid regexp literal"))))
    (goto-char (match-end 0))
    (let* ((end (match-end 1))
           (value (buffer-substring-no-properties
                   start end))
           (ignore-case (match-string 2)))
      (gosh-object (if ignore-case 'iregexp 'regexp) value))))

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
  (let* ((regexp gosh-reader--charset-re)
         (data (gosh-reader--read-by-regexp regexp 1)))
    (gosh-object 'charset data)))

;; FIXME refine
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
      (gosh-reader--read-special-string))
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
      ;; Introduces an interpolated string.
      (gosh-reader--read-special-string))
     ((eq begin ?\")
      ;; Introduces an interpolated string.
      ;; commit 386882ef9d5ac891addee7d2255a685432fb2e50
      (gosh-reader--read-string))
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

(defun gosh-reader--read-string/fallback ()
  ;; FIXME: double quoted string may have special sequence
  (let ((start (point)))
    (condition-case err
        ;; Read by Emacs reader for performance reason.
        (read (current-buffer))
      (error
       ;; fallback
       (let ((end (point)))
         (condition-case nil
             (progn
               (goto-char start)
               (gosh-reader--read-gauche-string))
           (error
            (goto-char end)
            (signal (car err) (cdr err)))))))))

(defun gosh-reader--read-gauche-string ()
  (unless (eq (char-after) ?\")
    (error "Not a string context"))
  (forward-char)
  (let* ((res '())
         (alist '((?\" . ?\") (?\\ . ?\\) (?n  . ?\n) (?r  . ?\r)
                  (?t  . ?\t) (?f  . ?\f) (?0  . ?\0)))
         (read-char
          (lambda ()
            (let ((c (char-after)))
              (cond
               ((eq c ?\\)
                (forward-char)
                (let ((c2 (char-after))
                      (cell))
                  (cond
                   ((setq cell (assq c2 alist))
                    (forward-char)
                    (cdr cell))
                   ((memq c2 '(?\s ?\n ?\t ?\r))
                    ;; Seems in `gosh-reader-ws' (?\v ?\f) is not
                    (unless (looking-at "[\s\t\r]*\n[\s\t\r]*")
                      (signal 'invalid-read-syntax '("Invalid newline escape")))
                    (goto-char (match-end 0))
                    nil)
                   ((eq c2 ?x)
                    ;; FIXME internal encoding (how/when get it?)
                    (forward-char)
                    (gosh-reader--read-hex 2))
                   ((eq c2 ?u)
                    ;; ucs-2
                    (forward-char)
                    (gosh-reader--read-ucs 4))
                   ((eq c2 ?U)
                    ;; ucs-4
                    (forward-char)
                    (gosh-reader--read-ucs 8)))))
               (t
                (forward-char)
                c)))))
         (char))
    (while (not (eq (char-after) ?\"))
      (setq char (funcall read-char))
      (when char
        (setq res (cons char res))))
    (forward-char)
    (concat (nreverse res))))

(byte-compile 'gosh-reader--read-string/fallback)
(byte-compile 'gosh-reader--read-gauche-string)

;;TODO is there performance problem? for a while testing
(defvar gosh-reader-exact-string nil)

(if gosh-reader-exact-string
    (fset 'gosh-reader--read-string 'gosh-reader--read-gauche-string)
  (fset 'gosh-reader--read-string 'gosh-reader--read-string/fallback))

(defun gosh-reader--read-datum ()
  (goto-char (match-end 0))
  (gosh-reader--word-to-datum (match-string-no-properties 1)))

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

(defun gosh-read-from-string (string)
  (let ((result (gosh-read-context-string string)))
    (cons (nth 0 result) (nth 1 result))))

(defun gosh-read-first-from-string (string)
  (car (gosh-read-from-string string)))

(defun gosh-read-all-from-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((res '()))
      (while (not (eobp))
        (let ((sexp (gosh-read)))
          (setq res (cons sexp res)))
        ;; to handle "AB CD;", "AB CD "
        (gosh-reader-ignore))
      (nreverse res))))

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
  :set 'gosh-default-set-command
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
COMMAND VERSION SYSLIBDIR LOAD-PATH TYPE PATH-SEPRATOR PATH-TO-EMACS PATH-FROM-EMACS"
  )

(defvar gosh-autoload-modules
  '(null user gauche scheme))

(defconst gosh-autoload-symbols-command-format
  (eval-when-compile
    (concat
     "(apply append (map (lambda (mod) "
     "(hash-table-map (module-table mod) "
     "(lambda (sym gloc) (string-copy (symbol->string sym))))) "
     "(fold (lambda (s r) (if-let1 m (find-module s) (cons m r) r)) '() '%s)))\n"
     )))

(defun gosh-autoload-symbols-command-string ()
  (format gosh-autoload-symbols-command-format gosh-autoload-modules))

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

;;TODO reconsider this. no need?
(defun gosh-register-command (command)
  (let ((full (gosh--check-command command)))
    (unless full
      (error "Unable recognize as gosh command %s" command))
    (let ((key (expand-file-name full)))
      (or (assoc key gosh-command-alist)
          (let* ((output (gosh--initialize-command->string full full "-V"))
                 (ver (when (string-match "version[\s\t]+\\([0-9][0-9.]+\\)" output)
                        (match-string 1 output)))
                 (type (cond
                        ((string-match "mingw32$" output) 'mingw32)
                        ((string-match "cygwin$" output) 'cygwin)
                        (t 'unix)))
                 (sep (cl-case type
                        ((mingw32) ";")
                        (t ":")))
                 (repo (let* ((res (gosh--initialize-command->string full "gauche-config" "--syslibdir"))
                              (res (substring res 0 -1))) ;remove trailing newline
                         (let* ((dir (file-name-directory res))
                                (dir2 (file-name-directory
                                       (directory-file-name dir))))
                           (directory-file-name dir2))))
                 (g2e (cl-case type
                        ((cygwin) 'gosh-cygpath->emacs-path)
                        (t 'identity)))
                 (e2g (cl-case type
                        ((cygwin) 'gosh-emacs-path->cygpath)
                        (t 'identity)))
                 (path (mapcar g2e (gosh-exact-load-path full t)))
                 (item (list
                        ;; FIXME:
                        ;;  NTEmacs break string contents which indicate exe path destructively.
                        ;;  e.g. c:/cygwin/bin/hoge.exe -> c:\cygwin\bin\hoge.exe
                        ;;  Do not use this string.
                        (copy-sequence key)
                        ver repo path type sep g2e e2g)))
            (setq gosh-command-alist
                  (cons item gosh-command-alist))
            item)))))

(defun gosh-switch-default-command (command)
  "Switch gosh command (ex: trunk <-> release)"
  (interactive
   (let ((command (completing-read
                   (format "Command %s -> " gosh-default-command-internal)
                   gosh-command-alist nil nil
                   gosh-default-command-internal)))
     (list command)))
  (gosh-default-initialize command))

(defun gosh-default-set-command (dummy value)
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

(defcustom gosh-force-from-scheme-mode nil
  "Force to `gosh-mode' no matter what file local variable \"mode:\" is."
  :type 'bool
  :group 'gosh-mode)

(defcustom gosh-mode-maybe-predicate-hook nil
  "Hook when call `gosh-force-from-scheme-mode' is non-nil.
All of this items is function with no-arg which return non-nil if you wish
to change `scheme-mode' to `gosh-mode'"
  :group 'gosh-mode
  :type 'hook)

(defvar gosh-force-mode-progress nil)

(defun gosh-mode-maybe-from-scheme-mode ()
  "Activate `gosh-mode' from `scheme-mode'"
  (when (and gosh-force-from-scheme-mode
             (eq major-mode 'scheme-mode)
             (not gosh-force-mode-progress)
             (dolist (pred gosh-mode-maybe-predicate-hook)
               (when (ignore-errors (save-excursion (funcall pred)))
                 (cl-return t))))
    (run-with-timer 0.1 nil 'gosh-mode-from-scheme-mode (current-buffer))))

(defun gosh-mode-maybe--from-file-name ()
  (and buffer-file-name
       (string-match "gauche\\|gosh" buffer-file-name)))

(defun gosh-mode-maybe--from-buffer-contents ()
  (goto-char (point-min))
  (re-search-forward "\\_<gauche\\_>" nil t))

(defun gosh-mode-maybe--from-shebang ()
  (goto-char (point-min))
  (when (looking-at auto-mode-interpreter-regexp)
    (let ((interpreter (match-string 2)))
      (string-match "gosh" interpreter))))

(add-hook 'gosh-mode-maybe-predicate-hook 'gosh-mode-maybe--from-file-name)
(add-hook 'gosh-mode-maybe-predicate-hook 'gosh-mode-maybe--from-buffer-contents)
(add-hook 'gosh-mode-maybe-predicate-hook 'gosh-mode-maybe--from-shebang)

(defun gosh-mode-from-scheme-mode (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'scheme-mode)
        (let ((gosh-force-mode-progress t))
          (gosh-mode))))))

(defun gosh--initialize-command->string (gosh command &rest args)
  (let ((dir (file-name-directory gosh)))
    (with-temp-buffer
      (apply 'call-process (expand-file-name command dir)
             nil (current-buffer) nil args)
      (buffer-string))))

(defun gosh--check-command (command)
  (let ((full (executable-find command)))
    (when (and full (string-match "/gosh\\(\\.exe\\)?$" full))
      full)))

(defconst gosh-call--eval-format
  "(receive vs %s (write vs))")

(defun gosh-call-process/eval (script &optional command)
  (with-temp-buffer
    (let ((args (list "-b" "-e" (format gosh-call--eval-format script))))
      (unless (= (apply 'gosh-call-process-1 command args) 0)
        (error "%s" (substring (buffer-string) 0 -1))))
    (goto-char (point-min))
    (let ((result (gosh-read)))
      (if (= (length result) 1)
          (car result)
        ;; import multiple values to emacs world
        (gosh-multivalues result)))))

(defun gosh-call-process (&rest args)
  (apply 'gosh-call-process-1 nil args))

(defun gosh-call-process-1 (command &rest args)
  (apply 'call-process (or command gosh-default-command-internal) nil t nil
         args))

(defun gosh-start-process (name buffer &rest args)
  (apply 'start-process name buffer
         gosh-default-command-internal
         args))

(defun gosh-available-modules ()
  "All module symbols in *load-path*"
  (let ((path (gosh-load-path)))
    (gosh-append-map
     (lambda (dir)
       (let ((len (length (directory-file-name dir))))
         (mapcar
          (lambda (f)
            (subst-char-in-string
             ?/ ?. (file-name-sans-extension (substring f (+ 1 len)))))
          (gosh-directory-tree-files dir "\\.scm$"))))
     path)))

(defun gosh-module->file (mod)
  ;;TODO gosh may allow non suffix file.
  (let* ((file (concat (subst-char-in-string ?. ?/ (symbol-name mod)) ".scm"))
         (dir
          (gosh-any-file-in-path
           file
           (gosh-load-path))))
    (when dir
      (expand-file-name file dir))))

(defun gosh-file->module (file &optional other-paths)
  (setq file (expand-file-name file))
  (catch 'found
    (dolist (path (append other-paths (gosh-load-path)))
      (let ((regexp (concat "\\`"
                            (regexp-quote (file-name-as-directory path))
                            "\\(.+\\)\\.scm\\'")))
        (when (string-match regexp file)
          (throw 'found
                 (subst-char-in-string
                  ?/ ?. (match-string 1 file))))))
    nil))

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
        (args '()))
    (when system-only
      (setenv "GAUCHE_LOAD_PATH" nil))
    (gosh-call-process/eval "*load-path*" command)))

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
;; Cached index
;;

;; maybe file path or lazy list
;;
(defcustom gosh-index-user-cache nil
  "testing introduce. List of user defined index. That hold path
 to cache that hold s-exp or index s-exp.
See `gosh-info-doc--env'"
  :group 'gosh-mode
  ;; TODO
  :type 'list)

(defun gosh-index--create-env (index)
  (pcase-exhaustive index
    ((pred stringp) ; guess as path
     (cond
      ((file-exists-p index)
       (with-temp-buffer
         (insert-file-contents index)
         (goto-char (point-min))
         (read (current-buffer))))
      (t
       (error "Not a supported index %s" index))))
    ((pred listp)
     index)))


;;
;; For cygwin path
;;

(defcustom gosh-cygwin-cygdrive nil
  "Path alias of Windows drive prefixed path in Cygwin.

c:/Windows == /cygdrive/c/Windows
d:/home == /cygdrive/d/home
"
  :group 'gosh-mode
  :type 'string
  :set (lambda (s v) (set s (file-name-as-directory v)))
  :initialize (lambda (s v) (set s v)))

(unless gosh-cygwin-cygdrive
  (setq gosh-cygwin-cygdrive
        (cond
         ((executable-find "cygpath")
          (with-temp-buffer
            (let ((drive (or (getenv "SystemDrive") "c:")))
              (call-process "cygpath" nil t nil "--unix" drive)
              (goto-char (point-min))
              (and (re-search-forward (format "^\\(/.*/\\)%s$" (substring drive 0 1)) nil t)
                   (match-string 1)))))
         (t "/cygdrive/"))))

(defcustom gosh-cygwin-directory nil
  "Cygwin installed directory."
  :group 'gosh-mode
  :type 'directory
  :set (lambda (s v) (set s (file-name-as-directory (expand-file-name v))))
  :initialize (lambda (s v) (set s v)))

(unless gosh-cygwin-directory
  (setq gosh-cygwin-directory
        (cond
         ((executable-find "cygpath")
          (with-temp-buffer
            (call-process "cygpath" nil t nil "--windows" "/")
            (goto-char (point-min))
            (buffer-substring (point-min) (line-end-position))))
         (t "c:\\cygwin"))))

(defun gosh-cygpath->emacs-path (path)
  (let ((cygdrive gosh-cygwin-cygdrive)
        (installed gosh-cygwin-directory)
        (case-fold-search t))
    (cond
     ((string-match
       (format "\\`\\(?:%s\\)\\([a-zA-Z]\\)/\\(.*\\)"
               (regexp-quote cygdrive)) path)
      (format "%s:/%s" (match-string 1 path) (match-string 2 path)))
     ((string-match "\\`/" path)
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
     ((string-match (concat "\\`" (regexp-quote installed) "\\(.*\\)") abspath)
      (concat "/" (match-string 1 abspath)))
     ((string-match "\\`\\([a-zA-Z]\\):/\\(.*\\)" abspath)
      (format "%s%s/%s" cygdrive (match-string 1 abspath) (match-string 2 abspath)))
     ((string-match "\\`/" abspath)
      (expand-file-name (substring abspath 1) installed))
     (t
      path))))


;;;
;;; Parse buffer
;;;

(defun gosh--parse-partial-sexp (from &optional to)
  (let ((first (point)))
    (prog1
        (parse-partial-sexp from (or to (point)))
      (unless (= first (point))
        (goto-char first)))))

;;TODO in rfc822-date->date call this command
(defun gosh-context-string-p (&optional point)
  (save-excursion
    (let ((context (parse-partial-sexp (point-min) (or point (point)))))
      (cond
       ((and context (nth 3 context))
        ;; handling gauche extend definition
        (let ((beg (nth 8 context)))
          (goto-char beg)
          (when (looking-back "#`?" (- (point) 2))
            (setq beg (match-beginning 0)))
          (cons (nth 3 context) beg)))
       ((looking-back "#`?" (- (point) 2))
        (cons ?\" (match-beginning 0)))))))

(defun gosh-context-comment-p (&optional point)
  (let ((context (gosh--parse-partial-sexp (point-min) point)))
    (when (and context (nth 4 context))
      (cons (nth 4 context) (nth 8 context)))))

(defun gosh-context-code-p (&optional point)
  (let ((context (gosh--parse-partial-sexp (point-min) point)))
    (and (not (nth 3 context))
         (not (nth 4 context)))))

(defun gosh-context-toplevel-p (&optional point)
  (not (nth 9 (gosh--parse-partial-sexp (point-min) point))))

;; goto beginning of current sexp
;; See `gosh-mode-test--BoL' at gosh-mode-test.el
(defun gosh-beginning-of-list ()
  (let ((first (point)))
    (gosh-beginning-of-string)
    (backward-up-list)
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

;; goto beginning of sexp if point is in sexp
;; sexp means string, symbol, list
;;TODO add test case
;; aa bb_ -> aa _bb
;; aa b_b -> aa _bb
;; aa _bb -> _aa bb
;;TODO rename is this a fundamental function?
;; move upward sexp.
(defun gosh-upward-sexp ()
  (unless (bobp)
    (or (gosh-beginning-of-string)
        (progn
          (skip-chars-backward gosh-reader-ws)
          (let* ((char (char-before))
                 (syn (char-syntax char)))
            (cond
             ((or (eq syn ?\() (memq char '(?\#)))
              (forward-char -1))
             (t
              (forward-sexp -1))))))))

;; returns current argument position within sexp
(defun gosh-beginning-of-current-sexp-operator ()
  (let ((pos 0))
    (skip-syntax-backward "w_")
    (while (and (not (bobp)) (not (eq ?\( (char-syntax (char-before)))))
      (gosh-upward-sexp)
      (cl-incf pos))
    pos))

(defun gosh-end-of-sexp ()
  (gosh-end-of-string)
  (let* ((c0 (char-before))
         (c (char-after))
         (syn0 (and c0 (char-syntax c0)))
         (syn (and c (char-syntax c))))
    (when (or (memq syn0 '(?\s ?>))
              (not (memq syn '(?\s ?>))))
      (ignore-errors (forward-sexp)))))

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
         (let ((pos2 (gosh-beginning-of-current-sexp-operator))
               (sym2 (gosh-parse-symbol-at-point)))
           (list sym1 pos1 sym2 pos2)))
       (list sym1 pos1 nil 0)))))


;;;
;;; parsing program which may contain unbalanced parenthese/bracket
;;;

(defun gosh-parse-symbol-at-point ()
  (save-excursion
    (let ((first (point)))
      (goto-char
       (condition-case nil
           (or (scan-sexps (point) -1) (point-min))
         (scan-error first)))
      ;;TODO make public gosh-reader--*
      (cond
       ((not (looking-at gosh-reader--word-re)) nil)
       ((or (>= (match-end 0) first)
            (progn (goto-char first)
                   (looking-at gosh-reader--word-re)))
        (gosh-reader--read-datum))
       (t nil)))))

(defun gosh-parse-last-expression-define-p ()
  "Return t if last expression is top-level define-*"
  (and (gosh-context-toplevel-p)
       (save-excursion
         (beginning-of-defun)
         (looking-at "([\s\t\n]*define"))))

;;TODO
;; (let* ([aa #f)) (valid-proc _))
;; Above case raise invalid-read-syntax error.
;; But what should i do? Should keep process as long as i can?
(defun gosh-parse--current-context (&optional max)
  (save-excursion
    (let ((res '())
          (prev nil)
          (comment (gosh-context-comment-p)))
      (cond
       (comment
        (goto-char (cdr comment)))
       (t
        (backward-prefix-chars)))
      (while (and (not (gosh-context-toplevel-p))
                  (or (null max)
                      (< (length res) max)))
        (let ((end (point)))
          (gosh-beginning-of-list)
          (let ((paren-start (char-after)))
            (backward-prefix-chars)
            (let* ((paren-end (gosh-paren-against-char paren-start))
                   (s (buffer-substring-no-properties (point) end))
                   ;; TODO workaround eval-when-compile to construct regexp?
                   (middle-of-dot (format "[.][%s]*\\'" gosh-reader-ws))
                   (sexp (gosh-read-first-from-string
                          (concat
                           s
                           (and (string-match middle-of-dot s)
                                "()")
                           `(,paren-end))))
                   (last (last sexp)))
              (when (and (consp last) prev)
                (setcdr last (list prev)))
              (setq res (cons sexp res))
              (setq prev sexp)))))
      res)))

(defun gosh-parse--current-fnsexp-in-list ()
  (save-excursion
    (gosh-end-of-sexp)
    (let* ((ctx (gosh-parse--current-context 1))
           (fnsexp (car-safe ctx))
           (index (if (/= (skip-chars-backward gosh-reader-ws) 0)
                      (gosh-length* fnsexp)
                    (1- (gosh-length* fnsexp)))))
      (unless (and (consp fnsexp)
                   (gosh-symbol-p (car fnsexp)))
        (setq fnsexp nil))
      (list fnsexp (max index 0)))))

(defun gosh-parse-current-module ()
  (save-excursion
    (save-restriction
      (widen)
      (catch 'return
        (gosh-end-of-string)
        ;; avoid starts of "(with-module"
        (let ((context (gosh-parse--current-context)))
          (dolist (module-switch '(define-module with-module))
            (let ((pair (assq module-switch context)))
              (when (and pair (nth 1 pair))
                (throw 'return (format "%s" (nth 1 pair)))))))
        (while (re-search-backward "^[\s\t]*(select-module[\s\t\n]+\\([^\s\t\n()]+\\)" nil t)
          (when (gosh-context-code-p)
            (throw 'return (match-string-no-properties 1))))
        ;; default is user module
        "user"))))

(defun gosh-parse-read-form ()
  ;; Read at least one expression.
  ;; Editing source code may have unbalanced brace.
  ;; if no more valid S-expression at point, cursor
  ;; ended the buffer and return nil.
  (let ((start (point)))
    (catch 'done
      (while (not (eobp))
        (condition-case nil
            (let ((sexp (gosh-read)))
              (when sexp
                (throw 'done sexp)))
          (error
           (goto-char start)
           (forward-line)
           ;;TODO in nested comment
           (unless (re-search-forward "^(" nil t)
             (goto-char (point-max))
             (throw 'done nil))
           (forward-line 0)
           (setq start (point))))))))

(defun gosh-parse-read-all ()
  (save-excursion
    (goto-char (point-min))
    (let ((res '())
          sexp)
      (while (setq sexp (gosh-parse-read-form))
        (setq res (cons sexp res)))
      (nreverse res))))

;;
;; extract from parsed
;;

(defun gosh-extract--match-clause-vars (x)
  (cond
   ((null x) '())
   ((gosh-symbol-p x)
    (if (memq x '(_ ___ ...))
        '()
      (list (list x))))
   ((consp x)
    (cl-case (car x)
      ((or not and)
       (gosh-extract--match-clause-vars (cdr x)))
      ((= $ @ struct)
       ;; TODO no need to check consp?
       (if (consp (cdr x)) (gosh-extract--match-clause-vars (cddr x)) '()))
      ((?\?)
       (gosh-extract--match-clause-vars (cddr x)))
      ((get! set!)
       (gosh-extract--match-clause-vars (cadr x)))
      ((quote) '())
      (t
       (gosh-append-map* 'gosh-extract--match-clause-vars x))))
   ((and (gosh-object-p x)
         (eq (gosh-object-type x) 'vector))
    (gosh-extract--match-clause-vars
     (append (gosh-object-value x) nil)))
   (t
    '())))

(defun gosh-extract-local-vars (&optional env)
  (let ((context (gosh-parse--current-context))
        ;; to carrying about alist as function argument
        (vars (list t)))
    (cl-loop for ctx on context
             do
             (pcase (car ctx)
               (`(lambda ,args . ,_)
                (gosh-extract--simple-args vars args))
               (`(match . ,clauses)
                (dolist (clause clauses)
                  (pcase clause
                    (`(,pat . ,_)
                     (gosh-extract--match-vars vars pat)))))
               (`(,(or 'match-let 'match-let*) ,exprs . ,_)
                (dolist (expr exprs)
                  (gosh-extract--match-vars vars (car-safe expr))))
               (`(match-let1 ,pat . ,_)
                (gosh-extract--match-vars vars pat))
               (`(let . ,let-args)
                (pcase let-args
                  (`(,(and (pred atom) named) ,bindings . ,_)
                     (gosh-extract--put
                      vars `(,named (lambda ,(mapcar (lambda (x) (car-safe x)) bindings)))))
                  (`(,(and (pred consp) bindings) . ,_)
                   (gosh-extract--let-vars vars bindings))))
               (`(,(or 'let* 'and-let* 'letrec 'fluid-let) ,bindings . ,_)
                (gosh-extract--let-vars vars bindings))
               (`(,(or 'let1 'rlet1 'if-let1 'and-let1) ,sym ,val . ,_)
                (gosh-extract--put-var vars sym (list val)))
               (`(,(or 'let-values 'let*-values) ,bindings . ,_)
                (dolist (var-expr bindings)
                  (gosh-extract--simple-args vars (car var-expr))))
               (`(,(or 'let-syntax 'letrec-syntax) ,bindings . ,_)
                (dolist (syn-expr bindings)
                  (pcase syn-expr
                    (`(,name ,transformer . ,_)
                     (gosh-extract--put-var vars name (gosh-extract-syntax transformer))))))
               (`(,(or 'do) ,exprs . ,_)
                ;;TODO try it
                (dolist (var-expr exprs)
                  (gosh-extract--put-var vars (car var-expr) nil)))
               (`(,(or 'receive) ,bindings . ,_)
                (gosh-extract--simple-args vars bindings))
               (`(,(or 'define 'define-constant 'define-inline) ,defined . ,_)
                ;;TODO if inner `define` definitions
                ;; (gosh-extract--put-var vars (car (nth 1 sexp))
                (pcase defined
                  (`(,name . ,args)
                   (gosh-extract--simple-args vars args))))
               (`(,(or 'define-in-module) ,module . ,form)
                (pcase form
                  (`((,name . ,args) . ,_)
                   ;;TODO test
                   (gosh-extract--simple-args vars args))))
               (`(,(or 'define-method) ,name ,args . ,_)
                (gosh-extract--typed-vars vars  args))
               ))
    (delq t vars)))

(defun gosh-extract--put-vars (alist key-value-pairs)
  (dolist (kv key-value-pairs)
    (gosh-extract--put-var alist (car kv) (cadr kv))))

(defun gosh-extract--put-var (alist key value)
  (let ((elm (assq key alist)))
    (unless elm
      (setq elm (list key nil))
      (setcdr (last alist) (list elm)))
    (setcdr elm value)
    alist))

(defun gosh-extract--put (alist form)
  (gosh-extract--put-var alist (nth 0 form) (nthcdr 1 form)))

(defun gosh-extract--match-vars (alist pat)
  (let ((vars (gosh-extract--match-clause-vars pat)))
    (dolist (v vars)
      (gosh-extract--put-var alist (car v) (cadr v)))))

(defun gosh-extract--let-vars (alist vars)
  (cl-loop for x in vars
           if (and (consp x) (gosh-symbol-p (car x)))
           do (gosh-extract--put-var alist (car x) nil)))

(defun gosh-extract--typed-vars (alist vars)
  (cl-loop for x in vars
           if (and (consp x) (gosh-symbol-p (car x)))
           do (gosh-extract--put-var alist (car x) (cdr x))
           else if (and (atom x) (gosh-symbol-p x))
           do (gosh-extract--put-var alist x nil)))

(defun gosh-extract--simple-args (alist vars)
  (cl-loop while vars
           do
           (pcase vars
             ((and (pred atom) var)
              (gosh-extract--put-var alist var nil))
             (`(,(and (pred keywordp) kwd) . ,_))
             (`(,(and (pred gosh-symbol-p) var) . ,_)
              (gosh-extract--put-var alist var nil))
             (`((,name . ,value) . ,_)
              (gosh-extract--put-var alist name value)))
           do (setq vars (and (consp vars)
                              (cdr vars)))))

(defun gosh-extract-import-symbols (importers &optional depth)
  (let ((res '()))
    (dolist (importer importers)
      (ignore-errors
        (let ((syms
               (pcase importer
                 (`(,module ,prefix)
                  (mapcar
                   (lambda (d)
                     (pcase d
                       (`(,(and (pred gosh-symbol-p) name) . ,body)
                        (let* ((dname (gosh-symbol-name name))
                               (fullsym (intern (concat prefix dname))))
                          (cons fullsym body)))
                       (asis asis)))
                   (gosh-env-resolve-exports module depth))))))
          (setq res (append syms res)))))
    res))

(defun gosh-extract-autoloads (forms)
  (let ((res '()))
    (dolist (sexp forms)
      (pcase sexp
        (`(autoload ,module . ,members)
         (let ((env (gosh-env-resolve-exports module)))
           (dolist (m members)
             (pcase m
               ((and (pred gosh-symbol-p) name)
                (pcase (assq name env)
                  (`(,_)
                   (pcase (assq module gosh-info-doc--env)
                     (`(,_ . ,docs)
                      (pcase (assq name docs)
                        (`(,_ ,body)
                         (setq res (cons (list name body) res)))
                        (_
                         (setq res (cons (list name) res)))))))
                  (`(,_ ,body)
                   (setq res (cons (list name body) res)))))))))))
    res))

(defun gosh-extract-importers (forms)
  ;; IMPORTERS ::= { IMPORTER } ;
  ;; IMPORTER  ::= MODULE , PREFIX ;
  (gosh-append-map 'gosh-extract--importer forms))

(defun gosh-extract--importer (sexp)
  (pcase sexp
    (`(,(or 'begin 'define-module) . ,body)
     (gosh-append-map 'gosh-extract--importer body))
    (`(cond-expand . ,body)
     (gosh-append-map 'gosh-extract--importer
                      (gosh-append-map 'cdr body)))
    (`(,(or 'use 'import) ,module . ,opts)
     (and-let* (((gosh-symbol-p module)))
       (let* ((gprefix (cadr (memq :prefix opts)))
              (prefix (cond
                       ((and gprefix (gosh-symbol-p gprefix))
                        (symbol-name gprefix))
                       ((stringp gprefix)
                        gprefix)
                       (t ""))))
         (list (list module prefix)))))
    (`(require ,mpath . ,_)
     (and-let* (((stringp mpath))
                (mname (subst-char-in-string ?/ ?. mpath))
                (module (intern mname)))
       (list (list module ""))))
    (`(require-extension . ,body)
     (gosh-append-map 'gosh-extract--importer body))))

(defun gosh-extract--slot-members (slots)
  (mapcar
   (lambda (x)
     (pcase x
       ((and (pred atom) name)
        (list name))
       (`(,name . ,_)
        (list name))
       (_ nil)))
   slots))

(defun gosh-extract-syntax (transformer)
  (pcase transformer
    (`(syntax-rules
       .
       ,(or `(,(and (pred gosh-symbol-p) ellipsis) ,_ . ,rules*)
            `(,_ . ,rules*)))
     (mapcar
      (lambda (r)
        (pcase r
          (`((_ . ,args) . ,_)
           ;; TODO FIXME should replace ellipsis in args
           `(syntax ,args))
          (_
           ;; FIXME reconsider it
           `(syntax unknown-syntax))))
      rules*))
    (_
     ;; FIXME no need support er-macro-transformer now. maybe too complex
     `(syntax))))

(defun gosh-extract-definition (sexp)
  (pcase sexp
    (`(define-syntax ,name ,transformer)
     `((,name ,@(gosh-extract-syntax transformer))))
    (`(define-macro (,name . ,args) . ,_)
     `((,name (syntax ,args))))
    (`(define-method ,name ,args . ,_)
     `((,name (lambda ,args))))
    (`(,(or 'define 'define-constant 'define-inline)
       ,first . ,body)
     (pcase first
       (`(,name . ,args)
        `((,name (lambda ,args))))
       ((pred atom)
        (pcase body
          (`(,value . ,_)
           `((,first ,value)))
          (_ nil)))
       (_ nil)))
    (`(define-class ,name ,super ,slots . ,_)
     ;; TODO format slots
     `((,name (class ,super ,slots))))
    (`(define-condition-type ,name ,super ,_pred . ,slots)
     ;; TODO format slots
     `((,name (class ,super ,slots))))
    (`(define-record-type ,type-spec ,_ctor ,_pred . ,members)
     (let ((members* (gosh-extract--slot-members members)))
       (pcase type-spec
         ((and (pred atom) name)
          `((,name (class nil ,members*))))
         (`(,name . _)
          `((,name (class nil ,members*))))
         (_
          nil))))
    (`((or 'begin 'begin0) . ,body)
     (gosh-append-map 'gosh-extract-definition body))
    (_
     '())))

(defun gosh-extract-base-modules (forms)
  (let ((extends (cdr
                  (or (assq 'extend forms)
                      (let ((def (assq 'define-module forms)))
                        (assq 'extend def))))))
    extends))

(defun gosh-extract-globals (forms &optional only-current)
  (let ((res '()))
    (dolist (sexp forms)
      (pcase sexp
        (`(define-module ,_ . ,body)
         (unless only-current
           (let ((parents (cdr (assq 'extend body))))
             (setq res (append
                        (gosh-append-map
                         'gosh-cache-module-global-env
                         parents)
                        res)))))
        (`(extend . ,parents)
         (unless only-current
           (setq res (append
                      (gosh-append-map
                       'gosh-cache-module-global-env
                       parents)
                      res))))
        (`(autoload ,module . ,members)
         (let ((env (gosh-env-resolve-exports module)))
           (setq res (append
                      (mapcar
                       (lambda (sym)
                         (or (assq sym env)
                             (list sym)))
                       members)
                      res))))
        (_
         (setq res
               (append
                (ignore-errors (gosh-extract-definition sexp))
                res)))))
    res))

;; -> ({(SOURCE . PUBLISH) | SOURCE} ...)
(defun gosh-extract-export-table (forms global-env &optional only-current)
  (let* ((res '())
         (extend-handler
          (lambda (form)
            (unless only-current
              (let ((parents (cdr form)))
                (setq res (append
                           (mapcar 'car
                                   (gosh-append-map
                                    'gosh-cache-module-export-env
                                    parents))
                           res))))))
         (export-handler
          (lambda (exports)
            (setq res (append
                       (mapcar
                        (lambda (item)
                          (pcase item
                            ((pred symbolp)
                             item)
                            (`(rename ,(and (pred gosh-symbol-p) source)
                                      ,(and (pred gosh-symbol-p) exported))
                             (cons source exported))
                            ;; Unknown
                            (_ nil)))
                        exports)
                       res))))
         (export-all-handler
          (lambda ()
            (setq res (append
                       (mapcar 'car global-env)
                       res)))))

    (ignore-errors
      (dolist (sexp forms)
        (pcase sexp
          (`(define-module ,_ . ,decls)
           (funcall extend-handler (assq 'extend decls))
           (cond
            ((and (consp decls) (assq 'export-all decls))
             (funcall export-all-handler))
            ((and (consp decls) (assq 'export decls))
             (funcall export-handler
                      (gosh-append-map*
                       (lambda (decl)
                         (pcase decl
                           (`(export . ,exports)
                            exports)))
                       decls)))))
          (`(,(or 'export 'export-if-defined) . ,exports)
           (funcall export-handler exports))
          (`(,(or 'export-all) . ,_)
           (funcall export-all-handler))
          (`(,(or 'extend) . ,_)
           (funcall extend-handler sexp)))))
    res))

(defun gosh-extract-exports (forms &optional only-current global-env)
  (let ((table (gosh-extract-export-table
                forms
                (or global-env (gosh-extract-globals forms nil))
                only-current)))
    (mapcar
     (lambda (x)
       (pcase x
         (`(,local . ,publish)
          publish)
         (symbol symbol)))
     table)))

(defun gosh-extract-exported-definitions (forms)
  (let* ((env (gosh-extract-globals forms))
         (exports (gosh-extract-exports forms t env))
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

(defconst gosh-extract-dynamic-load-exports-format
  (eval-when-compile
    (concat
     "(begin"
     "(dynamic-load \"%s\")"
     "(hash-table-map"
     "(module-table (find-module '%s)) (^ (s _) s)))")))

(defun gosh-extract-dynamic-library-exports (forms)
  (let ((res '())
        module)
    (dolist (form forms)
      (cond
       ((and (eq (car-safe form) 'dynamic-load) module)
        ;;TODO not consider module differences
        (let ((dll-name (nth 1 form)))
          (setq res
                (append
                 res
                 (gosh-call-process/eval
                  (format gosh-extract-dynamic-load-exports-format dll-name module))))))
       ((eq (car-safe form) 'select-module)
        ;; FIXME: may be appended to other module.
        (setq module (nth 1 form)))
       (t nil)))
    res))

;;
;; Parsed cache
;;

;;TODO to have extend relation
(defvar gosh-cache--module-export-env '(t)
  "Each item is following list
\\(file-name module-symbol time symbols-alist)

TODO key should be module-file?? multiple executable make complex.")

;;TODO to have extend relation
;;TODO refactor file information collect to one object. same *-module-exports
(defvar gosh-cache--file-global-env '(t)
  "Each item is following list
\\(file-name module-symbol time symbols-alist)")

(defun gosh-cache--get (cache)
  (nth 3 cache))

(defun gosh-cache--pull (file cache)
  (let ((hit (assoc file cache)))
    (cond
     ((null hit)
      (setq hit (cons file nil))
      (setcdr (last cache) (list hit)))
     ((ignore-errors
        (let ((mtime (gosh-file-mtime (nth 0 hit)))
              ;; cached time
              (ctime (nth 2 hit)))
          (> mtime ctime)))
      (setcdr hit nil))
     ((let ((buf (get-file-buffer (file-truename file))))
        (and buf (buffer-modified-p buf)))
      ;;TODO consider using :modtime property
      (setcdr hit nil)))
    hit))

(defun gosh-cache--push (file mod cache value)
  (let ((mtime (gosh-file-mtime file)))
    (setcdr cache (list mod mtime value))))

(defvar gosh-cache--file-forms '(t)
  "TODO comment ")

(defun gosh-cache-file-forms (file)
  (let* ((cached (gosh-cache--pull file gosh-cache--file-forms)))
    (or (gosh-cache--get cached)
        ;; (re)compute module as forms
        (let ((res (gosh-with-find-file file
                     (gosh-parse-read-all)))
              (module (gosh-file->module file)))
          (gosh-cache--push file module cached res)
          res))))

(defun gosh-cache-module-forms (module)
  (let ((file (gosh-module->file module)))
    (gosh-cache-file-forms file)))

;; Gather export symbols of MOD.
(defun gosh-cache-module-export-env (module)
  (let ((mname (symbol-name module)))
    (cond
     ((and (gosh-symbol-p module)
           (string-match "^srfi-\\([0-9]+\\)$" mname))
      (let ((srfi-n (string-to-number (match-string 1 mname))))
        (gosh-env--srfi-exports srfi-n)))
     (t
      (let* ((file (gosh-module->file module))
             (cached (and file
                          (gosh-cache--pull
                           file gosh-cache--module-export-env))))
        (or (gosh-cache--get cached)
            ;; (re)compute module exports
            (let* ((forms (gosh-cache-module-forms module))
                   (res (gosh-env-file-exports forms)))
              (gosh-cache--push file module cached res)
              res)))))))

;; Gather module all definitions which is derived by child module.
(defun gosh-cache-module-global-env (module)
  (let* ((file (gosh-module->file module))
         (cached (and file (gosh-cache--pull file gosh-cache--file-global-env))))
    (or (gosh-cache--get cached)
        ;; (re)compute module exports
        (let* ((forms (gosh-cache-module-forms module))
               (res (gosh-extract-globals forms t)))
          (gosh-cache--push file module cached res)
          res))))

;;TODO
(defun gosh-cache-lookup-symbol (symbol)
  )

;;
;; env
;;

(defcustom gosh-env-export-nested-depth 3
  "Max depth of export symbol is resolved."
  :group 'gosh-mode
  :type 'integer)

(defun gosh-env-resolve-exports (module &optional depth)
  (unless depth
    (setq depth 0))
  ;; Just avoid eternal recursion or slow down
  (when (<= depth gosh-env-export-nested-depth)
    (let* ((env (gosh-cache-module-export-env module))
           forms global-env table import-env
	         (resolve-import
            (lambda (name)
              (unless import-env
                (let ((importers (gosh-extract-importers forms)))
                  (setq import-env (gosh-extract-import-symbols
                                    importers (+ depth 1)))))
              (cdr-safe (assq name import-env)))))
      (mapcar
       (lambda (e)
         (pcase e
           (`(,(and (pred gosh-symbol-p) name) . ,bodies)
            (cond
             ((consp bodies)
              e)
             (t
              (unless forms
                (setq forms (gosh-cache-module-forms module))
                (setq global-env (gosh-extract-globals forms))
                (setq table (gosh-extract-export-table forms global-env t))
                )
              (let ((bodies (pcase (rassq name table)
                              (`(,source . ,_)
                               ;; found (export (rename source NAME)) form
                               (or (cdr-safe (assq source global-env))
                                   (funcall resolve-import source)))
                              (_
                               (funcall resolve-import name)))))
                (cons name bodies)))))))
       env))))

(defun gosh-env-file-exports (forms)
  (let ((defs (gosh-extract-exported-definitions forms))
        (modules (gosh-extract-base-modules forms)))
    (dolist (mod modules)
      (let ((file (gosh-module->file mod))
            (base-forms (gosh-cache-module-forms mod)))
        (setq defs (append
                    (gosh-env-file-exports base-forms)
                    defs))))
    defs))

(defun gosh-env-exports-functions (file)
  (let ((forms (gosh-cache-file-forms file)))
    (gosh-extract-exports forms nil nil)))

(defun gosh-env--srfi-exports (i)
  (cond
   ((integerp i)
    (when (and (>= i 0)
               (< i (length *gosh-scheme-srfi-info*)))
      (cdr (aref *gosh-scheme-srfi-info* i))))))

(defun gosh-env-current (&optional for-completion)
  ;; r5rs
  (let* ((env (list *gosh-scheme-r5rs-info* *gosh-undocumented-info*))
         (forms (or
                 (and buffer-file-name
                      (gosh-cache-file-forms buffer-file-name))
                 (gosh-parse-read-all)))
         (importers (gosh-extract-importers forms)))
    ;; language basics
    (let ((base (ignore-errors
                  (gosh-info-doc-env (if for-completion t importers)))))
      (when base
        (push base env)))
    ;; User cached index
    (when for-completion
      (dolist (index gosh-index-user-cache)
        (let ((cache (gosh-index--create-env index)))
          (dolist (c cache)
            (push (cdr-safe c) env)))))
    ;; imports
    (let ((imports (ignore-errors
                     (gosh-extract-import-symbols importers))))
      (when imports
        (push imports env)))
    ;; top-level defs
    (let ((top (ignore-errors (gosh-extract-globals forms nil))))
      (when top
        (push top env)))
    ;; autoload defs
    (let ((autoload (ignore-errors (gosh-extract-autoloads forms))))
      (when autoload
        (push autoload env)))
    ;; current local vars
    (let ((locals (ignore-errors (gosh-extract-local-vars env))))
      (when locals
        (push locals env)))
    env))

(defmacro gosh-env-map-symbol (env var &rest form)
  (declare (indent 2))
  `(mapc
    (lambda (env)
      (mapc
       (lambda (,var)
         (when (gosh-symbol-p (car ,var))
           ,@form))
       env))
    ,env))

(defun gosh-env-filter (pred env)
  (mapcar 'car
          (apply 'concatenate
                 'list
                 (mapcar (lambda (e) (gosh-filter pred e)) env))))

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

;;TODO SYM is gosh-object
(defun gosh-env-lookup (env sym)
  (let ((specs (gosh-env-matches env sym nil))
        res s)
    ;; find most detailed info
    (dolist (s specs)
      (cond
       ((not (consp s)))
       ((or
         (> (length s) (length res))
         (and (= (length s) (length res))
              ;;FIXME TODO refactor....
              (consp (cadr s))
              (consp (cadr res))
              (eq 'lambda (caadr s))
              (eq 'lambda (caadr res))
              ;; compare lambda args
              (> (length (gosh-flat (cadadr s)))
                 (length (gosh-flat (cadadr res))))))
        ;;TODO return immediately
        (setq res s))))
    res))


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

;;TODO reconsider it
;;;###autoload
(defun gosh-run (cmd)
  "Wrapper of `run-scheme' command."
  (interactive
   (let ((command
          (if current-prefix-arg
              (completing-read "Run Gosh: "
                               ;;TODO wrong!
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

(defface gosh-momentary-warning-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to momentary warning."
  :group 'gosh-mode)

(defun gosh-momentary-message (msg)
  "Show temporary message to current point.
Import from mew-complete.el"
  (gosh-momentary-message-0 msg 'gosh-momentary-message-face))

(defun gosh-momentary-warning (msg)
  "Show temporary warning to current point.
Import from mew-complete.el"
  (gosh-momentary-message-0 msg 'gosh-momentary-warning-face))

(defun gosh-momentary-message-0 (msg face)
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
      (gosh-momentary-message--overlay start end face)
      (sit-for wait-msec)
      (delete-region start end)
      (set-buffer-modified-p modified)
      (delete-overlay gosh-momentary-message--overlay)
      (when quit-flag
        (setq quit-flag nil)
        (setq unread-command-events (list 7))))))

(defun gosh-momentary-message--overlay (start end face)
  (let ((ov gosh-momentary-message--overlay))
    (unless ov
      (setq ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face (or face 'gosh-momentary-message-face))
    (move-overlay ov start end (current-buffer))
    (setq gosh-momentary-message--overlay ov)))


;;;
;;; eldoc
;;;

(require 'eldoc nil t)
;;TODO
;; eldoc error: (error Lisp nesting exceeds `max-lisp-eval-depth')
;; at <twitter-cred>

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
  (when (featurep 'eldoc)
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

(defun gosh-eldoc--highlight-syntax (info-sexp highlight)
  (pcase-exhaustive highlight
    (`(,index . ,options)
     (let ((target (nth index info-sexp)))
       (unless target
         (if-let ((it (gosh-eldoc--sexp-rest-arg info-sexp)))
             (setq target it)))
       (concat
        "("
        (mapconcat
         (lambda (rule)
           (let ((str (gosh-eldoc--object->string rule)))
             (when (string= str "")
               (setq str (with-output-to-string
                           (princ rule))))
             (when (eq target rule)
               (add-text-properties 0 (length str)
                                    (list 'face 'eldoc-highlight-function-argument)
                                    str))
             str))
         info-sexp
         " ")
        ")")))))

(defun gosh-eldoc--highlight-fn (info-sexp highlight)
  (pcase-exhaustive highlight
    (`(,index ,sym . ,options)
     (let* ((real-sexp (gosh-eldoc--normalize-fn-sexp info-sexp))
            (prev-sym (car options))
            (kwd (or (and (keywordp sym) sym)
                            (and (keywordp prev-sym) prev-sym)))
            target)
       (when kwd
         (let ((kwd-sym
                (intern-soft (substring (symbol-name kwd) 1)))
               (keywords (cdr (memq :key info-sexp))))
           (setq target (or
                         ;; VARIABLE
                         ;; (VARIABLE INIT-EXPR)
                         ;; ((KEYWORD VARIABLE) INIT-EXPR)
                         (assoc kwd keywords
                                (lambda (cell _)
                                  (pcase cell
                                    (`(,(and (pred keywordp) k) . ,_)
                                     (eq kwd k))
                                    ((and (pred symbolp) s)
                                     (eq kwd-sym s))
                                    (`(,(and (pred symbolp) s) . ,_)
                                     (eq kwd-sym s)))))))))
       (unless target
         (setq target (nth index real-sexp)))
       ;; index exceed maximum but
       ;; * (lambda args)
       ;; * (lambda (:rest args))
       (unless target
         (if-let ((sexp (gosh-eldoc--sexp-rest-arg real-sexp)))
             (setq target sexp)))
       (concat
        "("
        (mapconcat
         (lambda (exp)
           (let ((str (gosh-eldoc--object->string exp)))
             (when (eq target exp)
               (add-text-properties
                0 (length str)
                (list 'face 'eldoc-highlight-function-argument)
                str))
             str))
         info-sexp
         " ")
        ")")))))

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
  (cl-case (car-safe x)
    ((string list) (car x))
    ((set) (or (cadr x) (car x)))
    ((flags) 'integer)
    ((lambda) 'procedure)
    ((syntax) 'syntax)
    (t x)))

(defun gosh-eldoc--canonicalize-order (ls)
  ;; put optional arguments inside brackets (via a vector)
  (cond
   ((memq :optional ls)
    (let ((res '())
          (opts '())
          (kwds '()))
      (while ls
        (pcase ls
         (`(:optional . ,ls*)
          (while (and (consp ls*)
                      (not (keywordp (car ls*))))
            (setq opts (cons (car ls*) opts))
            (setq ls* (cdr ls*)))
          (setq ls ls*))
         (`(:key . ,ls*)
          (unless (consp kwds)
            (setq kwds (cons :key kwds)))
          (while (and (consp ls*)
                      (not (keywordp (car ls*))))
            (setq kwds (cons (car ls*) kwds))
            (setq ls* (cdr ls*)))
          (setq ls ls*))
         (`((pred keywordp) . ,ls*)
          (setq ls ls*))
         (`(,item . ,ls*)
          (setq ls ls*)
          (setq res (cons item res)))))
      (append (nreverse res)
              (mapcar 'vector (nreverse opts))
              (nreverse kwds))))
   (t
    ls)))

(defun gosh-eldoc--sexp-rest-arg (sexp)
  (catch 'found
    (dolist (x sexp)
      (when (and (vectorp x)
                 (> (length x) 0)
                 (let ((sym (aref x 0)))
                   (and (gosh-symbol-p sym)
                        (string-match "\\.\\.\\.\\'" (gosh-symbol-name sym)))))
        (throw 'found x)))
    ;; vector SEXP to list
    (let ((last (car (last (append sexp nil)))))
      (when (and last
                 (gosh-symbol-p last)
                 (string-match "\\.\\.\\.\\'" (gosh-symbol-name last)))
        (throw 'found last)))
    nil))

(defun gosh-eldoc--translate-dot-cell (ls)
  (let ((res '()))
    (while (consp ls)
      (setq res (cons (car ls) res))
      (setq ls (cdr ls)))
    (when (not (null ls))
      (let ((disp (concat (gosh-eldoc--object->string ls) "...")))
        (setq res (cons (vector (intern disp)) res))))
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
        (let* ((fnsexp (gosh-parse--current-fnsexp-in-list))
               (fnpos (nth 1 fnsexp))
               (sym (gosh-nth* fnpos (car fnsexp)))
               (fnsym (caar fnsexp))
               (env (save-excursion
                      (gosh-beginning-of-string)
                      (gosh-env-current)))
               (spec1 (and fnsym (gosh-env-matches env fnsym t)))
               (spec2 (and sym (gosh-env-matches env sym t)))
               (string1 (and spec1
                             (gosh-eldoc--find-and-print-strings
                              spec1 env
                              (cons fnpos (nthcdr (1- fnpos) (car fnsexp))))))
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
   ((gosh-symbol-p obj)
    (gosh-symbol-name obj))
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
  (pcase-exhaustive spec
    ((pred atom) nil)
    (`(,name ,type . ,args)
     (concat
      (pcase-exhaustive type
        (`(class ,super ,slots)
         (concat
          "Class: "
          (gosh-eldoc--object->string name)
          ;;super class
          (when super
            (concat
             " Base: "
             (gosh-eldoc--object->string super)))
          (when slots
            (concat
             " Slots: "
             (gosh-eldoc--object->string
              (mapcar 'car-safe slots))))))
        (`(syntax ,args)
         (concat
          "Syntax: "
          (let ((sexp
                 (cons name
                       (gosh-eldoc--canonicalize-order
                        (mapcar 'gosh-eldoc--base-type
                                (gosh-eldoc--translate-dot-cell args))))))
            (if highlight
                (gosh-eldoc--highlight-syntax sexp highlight)
              (gosh-eldoc--object->string sexp)))))
        (`(lambda ,args)
         (concat
          (let ((sexp
                 (cons name
                       (gosh-eldoc--canonicalize-order
                        (mapcar 'gosh-eldoc--base-type
                                (gosh-eldoc--translate-dot-cell args))))))
            (if highlight
                (gosh-eldoc--highlight-fn sexp highlight)
              (gosh-eldoc--object->string sexp)))))
        (`(,(and (or 'parameter 'variable 'constant) keytype))
         (format "%s: " (capitalize (symbol-name keytype))))
        ((pred stringp)
         (concat
          "String: \""
          (gosh-eldoc--object->string type)
          "\""))
        ((pred numberp)
         (concat
          "Number: "
          (gosh-eldoc--object->string type)))
        ((pred gosh-symbol-p)
         (let ((spec (gosh-env-lookup env type)))
           ;; ignore `max-lisp-eval-depth' error
           (or (gosh-eldoc--find-and-print-string spec env highlight)
               (format "some parameter or alias typed `%s'" type))))
        ((pred gosh-object-p)
         (concat
          (capitalize (symbol-name (gosh-object-type type)))
          ": "
          (gosh-eldoc--object->string (gosh-object-value type))))
        (_
         (gosh-eldoc--object->string type)))))
    (`(,_)
     "Some of ( Parameter | Alias | Dynamic loaded procedure )")))


;;;
;;; Completion
;;;

(defun gosh-complete-expand (str coll &optional strs pred)
  (let* ((coll (mapcar (lambda (x)
                         (cond
                          ((gosh-symbol-p x) (list (gosh-symbol-name x)))
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
      (gosh-momentary-warning "[No completion]"))
     ((not (string= str completion))
      (let ((all (all-completions str (append strs coll) pred))
            (prefix-p (gosh-string-prefix-p completion completion1)))
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

;; checking return values:
;;   a should be capable of returning instances of b
(defun gosh-complete--type-match-p (a b)
  (let ((a1 (gosh-complete--guess-type a))
        (b1 (gosh-complete--guess-type b)))
    (cond
     ((eq a1 'undefined) nil)        ; check a *does* return something
     ((eq a1 b1) t)                  ; and they're the same
     ((eq a1 'object) t)             ; ... or a can return anything
     ((eq b1 'object) t)             ; ... or b can receive anything
     ((symbolp a1)
      (if (symbolp b1)
          (cl-case a1                      ; ... or the types overlap
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
                (gosh-complete--type-match-p
                 a1 (gosh-complete--guess-type x)))
              (cdr b1))
           (let ((b2 (gosh-complete--translate-special b1)))
             (and (not (equal b1 b2))
                  (gosh-complete--type-match-p a1 b2)))))))
     ((consp a1)
      (cl-case (car a1)
        ((or)
         ;; type unions
         (gosh-find
          (lambda (x)
            (gosh-complete--type-match-p (gosh-complete--guess-type x) b1))
          (cdr a1)))
        ((lambda)
         ;; procedures
         (or (eq 'procedure b1)
             (and (consp b1)
                  (eq 'lambda (car b1))
                  (gosh-complete--param-list-match-p (cadr a1)
                                                     (cadr b1)))))
        (t
         ;; other special types
         (let ((a2 (gosh-complete--translate-special a1))
               (b2 (gosh-complete--translate-special b1)))
           (and (or (not (equal a1 a2)) (not (equal b1 b2)))
                (gosh-complete--type-match-p a2 b2)))))))))

(defun gosh-complete--lookup-type (spec pos)
  (let ((i 1)
        (type nil))
    (while (and (consp spec) (<= i pos))
      (cond
       ((eq :optional (car spec))
        (when (and (= i pos) (consp (cdr spec)))
          (setq type (cadr spec)))
        (setq i (+ pos 1)))
       ((= i pos)
        (setq type (car spec))
        (setq spec nil))
       ((and (consp (cdr spec)) (eq '... (cadr spec)))
        (setq type (car spec))
        (setq spec nil)))
      (setq spec (cdr spec))
      (cl-incf i))
    (and type
         (gosh-complete--guess-type type))))

(defun gosh-complete--param-list-match-p (p1 p2)
  (or (gosh-symbol-p p1)
      (gosh-symbol-p p2)
      (and (null p1) (null p2))
      (and (consp p1) (consp p2)
           (gosh-complete--param-list-match-p (cdr p1) (cdr p2)))))

(defun gosh-complete--translate-special (x)
  (cl-case (car-safe x)
    ((list string) (car x))
    ((set) (cadr x))
    ((flags) 'integer)
    (t x)))

(defun gosh-complete-symbol-name-at-point ()
  (let ((end (point))
        (start (save-excursion
                 (skip-syntax-backward "w_")
                 (point))))
    (buffer-substring-no-properties start end)))

(defun gosh-complete-file-name (_ sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir "."))))
    (if dir
        (mapcar (lambda (f) (concat dir f)) res)
      res)))

(defun gosh-complete-directory-name (_ sym)
  (let* ((file (file-name-nondirectory sym))
         (dir (file-name-directory sym))
         (res (file-name-all-completions file (or dir ".")))
         (res2 (if dir (mapcar (lambda (f) (concat dir f)) res) res)))
    (gosh-filter 'file-directory-p res2)))

(defun gosh-complete-in-string (type)
  (pcase type
    ('filename
     '(gosh-complete-file-name file-name-nondirectory))
    ('directory
     '(gosh-complete-directory-name file-name-nondirectory))
    (`(string ,str)
     str)
    (`(or ,types)
     (car (delete nil (mapcar 'gosh-complete-in-string types))))))

(defun gosh-complete-apply-string-completer (cmpl sym)
  (let ((func (if (consp cmpl) (car cmpl) cmpl))
        (trans (and (consp cmpl) (cadr cmpl))))
    (funcall func trans sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is rather complicated because we want to auto-generate
;; docstring summaries from the type information, which means
;; inferring various types from common names.  The benefit is that you
;; don't have to input the same information twice, and can often
;; cut&paste&munge procedure descriptions from the original
;; documentation.

;;TODO make obsolete.
;; this function detect type by name of symbol. fuck!
(defun gosh-complete--guess-type (type)
  (cond
   ((not (gosh-symbol-p type))
    type)
   (t
    (let ((name (gosh-symbol-name type)))
      ;; (cl-case type
      ;;   ((pred proc thunk handler dispatch producer consumer f fn g kons)
      ;;    'procedure)
      ;;   ((num) 'number)
      ;;   ((z) 'complex)
      ;;   ((x1 x2 x3 y timeout seconds nanoseconds) 'real)
      ;;   ((i j k n m int index size count len length bound nchars start end
      ;;       pid uid gid fd fileno errno)
      ;;    'integer)
      ;;   ((ch) 'char)
      ;;   ((str name pattern) 'string)
      ;;   ((file path pathname) 'filename)
      ;;   ((dir dirname) 'directory)
      ;;   ((sym id identifier) 'symbol)
      ;;   ((ls lis lst alist lists) 'list)
      ;;   ((vec) 'vector)
      ;;   ((exc excn err error) 'exception)
      ;;   ((ptr) 'pointer)
      ;;   ((bool) 'boolean)
      ;;   ((env) 'environment)
      ;;   ((char string boolean number complex real integer procedure char-set
      ;;          port input-port output-port pair list vector array stream hash-table
      ;;          thread mutex condition-variable time exception date duration locative
      ;;          random-source state condition condition-type queue pointer
      ;;          u8vector s8vector u16vector s16vector u32vector s32vector
      ;;          u64vector s64vector f32vector f64vector undefined symbol
      ;;          block filename directory mmap listener environment non-procedure
      ;;          read-table continuation blob generic method class regexp regmatch
      ;;          sys-stat fdset)
      ;;    type)
      ;;   ((parent seed option mode) 'non-procedure)
      (cond
       ((string-match "file\\|path" name) 'filename)
       ((string-match "dir\\|directory\\|dirname" name) 'filename)
       (t
        (let* ((i (string-match "-?[0-9]+$" name)))
          ;;TODO "nil-1" fufufu
          (cond
           (i
            (gosh-complete--guess-type (intern (substring name 0 i))))
           ((setq i (string-match "-\\([^-]+\\)$" name))
            (gosh-complete--guess-type (intern (substring name (+ i 1)))))
           ((string-match "\\?$" name)
            'boolean)
           (t
            'object)))))))))

(defun gosh-smart-complete (&optional arg)
  (interactive "P")
  (let ((sym (gosh-complete-symbol-name-at-point))
        (in-str-p (gosh-context-string-p)))
    (cl-destructuring-bind
        (env inner-proc inner-pos outer-proc outer-pos)
        (save-excursion
          (if in-str-p (gosh-beginning-of-string))
          (cons (gosh-env-current t) (gosh-enclosing-2-sexp-prefixes)))
      (let* ((outer-spec (gosh-env-lookup env outer-proc))
             (outer-type (gosh-complete--guess-type (cadr outer-spec)))
             (inner-spec (gosh-env-lookup env inner-proc))
             (inner-type (gosh-complete--guess-type (cadr inner-spec))))
        (cond
         ;; return all env symbols when a prefix arg is given
         (arg
          (gosh-complete-expand sym (gosh-env-filter (lambda (x) t) env)))
         ;; allow different types of strings
         (in-str-p
          (let* ((param-type
                  (and (consp inner-type)
                       (eq 'lambda (car inner-type))
                       (gosh-complete--lookup-type (cadr inner-type) inner-pos)))
                 (completer (or (gosh-complete-in-string param-type)
                                '(gosh-complete-file-name
                                  file-name-nondirectory))))
            (gosh-complete-expand
             ;;(if (consp completer) (funcall (cadr completer) sym) sym)
             sym
             (gosh-complete-apply-string-completer completer sym))))
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
                               (gosh-complete--lookup-type (cadr inner-type) inner-pos)))
                          (and (consp param-type)
                               (eq 'lambda (car param-type))
                               (eq (caddr inner-type) (caddr param-type)))))))
          (let ((want-type (gosh-complete--lookup-type (cadr outer-type) outer-pos)))
            (gosh-complete-expand
             sym
             (gosh-env-filter
              (lambda (x)
                (let ((type (cadr x)))
                  (or (memq type '(procedure object nil))
                      (and (consp type)
                           (or (and (eq 'syntax (car type))
                                    (not (eq 'undefined (caddr type))))
                               (and (eq 'lambda (car type))
                                    (gosh-complete--type-match-p (caddr type)
                                                                 want-type)))))))
              env))))
         ;; completing a normal parameter
         ((and inner-proc
               (not (zerop inner-pos))
               (consp inner-type)
               (memq (car inner-type) '(lambda)))
          (let* ((param-type (gosh-complete--lookup-type (cadr inner-type) inner-pos))
                 (set-or-flags
                  (or (pcase param-type
                        (`(set ,_ . ,value) value)
                        (`(flags ,value) value))
                      ;; handle nested arithmetic functions inside a flags
                      ;; parameter
                      (and (not (zerop outer-pos))
                           (consp outer-type)
                           (memq (car outer-type) '(lambda))
                           (let ((outer-param-type
                                  (gosh-complete--lookup-type (cadr outer-type)
                                                              outer-pos)))
                             (and (consp outer-param-type)
                                  (eq 'flags (car outer-param-type))
                                  (memq (gosh-complete--guess-type param-type)
                                        '(number complex real rational integer))
                                  (memq (gosh-complete--guess-type (caddr inner-type))
                                        '(number complex real rational integer))
                                  (cdr outer-param-type))))))
                 (base-type (if set-or-flags
                                (if (and (consp param-type)
                                         (eq 'set (car param-type)))
                                    (gosh-complete--guess-type (cadr param-type))
                                  'integer)
                              param-type))
                 (base-completions
                  (gosh-env-filter
                   (lambda (x)
                     (and (not (and (consp (cadr x)) (eq 'syntax (caadr x))))
                          (gosh-complete--type-match-p (cadr x) base-type)))
                   env))
                 (str-completions
                  (let ((completer (gosh-complete-in-string base-type)))
                    (and
                     completer
                     (gosh-complete-apply-string-completer completer sym)))))
            (gosh-complete-expand
             sym
             (append set-or-flags base-completions)
             str-completions)))
         ;; completing a function
         ((zerop inner-pos)
          (gosh-complete-expand
           sym
           (gosh-env-filter
            (lambda (x)
              (or (null (cdr x))
                  (and (cadr x) (atom (cadr x)))
                  (memq (cadr x) '(procedure object nil))
                  (and (consp (cadr x))
                       (memq (caadr x) '(lambda syntax parameter)))))
            env)))
         ;; complete everything
         (t
          (gosh-complete-expand sym (gosh-env-filter (lambda (x) t) env))))))))

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

      (ac-define-source gosh-parameters
        '((candidates . gosh-ac-parameter-candidates)
          (symbol . "p")
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
          `((candidates . gosh-available-modules)
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
    (add-to-list 'ac-sources 'ac-source-gosh-parameters)
    (add-to-list 'ac-sources 'ac-source-gosh-symbols)
    (add-to-list 'ac-sources 'ac-source-gosh-keywords)
    (add-to-list 'ac-sources 'ac-source-gosh-modules)))

(defun gosh-ac-inferior-candidates ()
  (gosh-inferior-symbol-candidates))

;; keywords match to current context
(defun gosh-ac-keywords-candidates ()
  (let ((fnsym (gosh-parse--current-fnsexp-in-list)))
    (when (and (nth 1 fnsym) (> (nth 1 fnsym) 0))
      (let ((fn (caar fnsym))
            (env (gosh-env-current)))
        (cond
         ((eq fn 'make)
          (gosh-ac-keywords-class-slot-candidates fnsym env))
         (t
          (gosh-ac-keywords-generic-candidates fn fnsym env)))))))

(defun gosh-ac-keywords-class-slot-candidates (fnsym env)
  (let ((class (cadar fnsym))
        res)
    (when class
      (gosh-env-map-symbol env c
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
    (gosh-env-map-symbol env c
      (let ((name (car c))
            (body (nth 1 c)))
        (when (and (eq name fn)
                   (consp body)
                   (memq (car body) '(lambda syntax)))
          (let ((key-args (cdr (member :key (cadr body)))))
            (mapcar (lambda (k)
                      (setq res (cons
                                 (gosh-ac-symbol->keyword-string
                                  (if (consp k) (car k) k))
                                 res)))
                    key-args)))))
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
  (let ((env (gosh-env-current))
        (res '()))
    (gosh-env-map-symbol env inf
      (setq res (cons (symbol-name (car inf)) res)))
    res))

;; symbols that have lambda expression
(defun gosh-ac-function-candidates ()
  (let ((env (gosh-env-current t))
        (res '()))
    (gosh-env-map-symbol env inf
      (let ((body (cadr inf)))
        (when (and (consp body)
                   (eq (car body) 'lambda))
          (setq res (cons (symbol-name (car inf)) res)))))
    res))

(defun gosh-ac-parameter-candidates ()
  (let ((env (gosh-env-current t))
        (res '()))
    (gosh-env-map-symbol env inf
      (let ((body (cadr inf)))
        (when (and (consp body)
                   (eq (car body) 'parameter))
          (setq res (cons (symbol-name (car inf)) res)))))
    res))


;;;
;;; Parenthese / Bracket handling (from quack)
;;;

(defun gosh-paren--balance-all-paren ()
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
            (while (re-search-forward "[\[\(]" nil t)
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

(defun gosh-paren--insert-closing (force default-close)
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
          (when (and closing
                     (not (eq actual-open opening)))
            (delete-region (1- (point)) (point))
            (insert closing)))
        (gosh-paren--balance-all-paren)))))
  (when blink-paren-function
    (funcall blink-paren-function)))

(defun gosh-paren-insert-close (&optional force)
  "Close opening parenthese or bracket.
Arg FORCE non-nil means forcely insert parenthese."
  (interactive "P")
  (gosh-paren--insert-closing force ?\)))

(defun gosh-paren-insert-close-bracket (&optional force)
  "Close opening parenthese or bracket.
Arg FORCE non-nil means forcely insert bracket."
  (interactive "P")
  (gosh-paren--insert-closing force ?\]))

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

(defun gosh-paren--insert-open (force default-open)
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
        (gosh-paren--switch-open)
        ;; move to after inserted point
        (forward-char))
       ((eq state 'opening)
        (when (save-excursion
                (backward-char)
                (when (gosh-paren--bracket-p)
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

(defun gosh-paren--switch-open ()
  (let ((open (char-after))
        (next (gosh--scan-sexps (point) 1)))
    (when (and next (eq (char-syntax open) ?\())
      (let* ((close (char-before next))
             (against (gosh-paren-against-char close)))
        (unless (eq against open)
          (delete-char 1)
          (insert against)
          (backward-char))))))

(defun gosh-paren-insert-open (&optional force)
  "Insert opening paren and notify if there is unmatched bracket."
  (interactive "P")
  (gosh-paren--insert-open force ?\())

(defun gosh-paren-insert-open-bracket (&optional force)
  "Insert opening bracket and notify if there is unmatched parenthese."
  (interactive "P")
  (gosh-paren--insert-open force ?\[))

;; TODO move to gosh-config
(defvar gosh-paren--auto-bracket-alist
  '(
    (fluid-let (*))
    (do (*))
    (let (*))
    (let* (*))
    (glet (*))
    (glet* (*))
    (let gosh-symbol-p (*))             ; named let
    (letrec (*))
    (and-let* (*))
    (let-values (*))
    (let*-values (*))
    (parameterize (*))
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
    (rxmatch-cond *)

    (let-args t (*))
    (parse-options t (*))

    (let-keywords t (*))
    (let-keywords* t (*))
    )
  "TODO
`*' match to every member of the paren context.
`?' match to a sexp.
`t' simply skip the context to auto bracket process.
")

;; `*' point to the cursor position.
(defun gosh-paren--context-bracket-p (context)
  (let (match-to)
    (let ((match-to
           (lambda (def args)
             (cl-loop for a1 in def
                      for a2 on args
                      if (eq a1 '*)
                      return (member '* a2)
                      else if (eq a1 '\?)
                      return (eq (car-safe a2) '*)
                      else if (listp a1)
                      return (funcall match-to a1 (car a2))
                      else if (and (functionp a1)
                                   (not (funcall a1 (car a2))))
                      return nil))))
      (let ((proc (car context)) (args (cdr context)))
        (cl-loop for (def-name . def-args) in gosh-paren--auto-bracket-alist
                 if (and (eq def-name proc)
                         (funcall match-to def-args args))
                 return t)))))

(defun gosh-paren--next-context (&optional count)
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
                   (cl-decf c)
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
             (sexp (gosh-read-first-from-string
                    ;; Add special character "*" at point
                    (concat partial " *" closing))))
        (and (consp sexp) sexp)))))

(defun gosh-paren--bracket-p ()
  ;; retry 5 count backward current sexp
  (ignore-errors
    (cl-loop for i from 0 to 5
             if (let ((context (gosh-paren--next-context i)))
                  (and context (gosh-paren--context-bracket-p context)))
             return t)))


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
  (condition-case err
      (gosh-smart-indent-1 indent-point state)
    (error
     (message "gosh-mode error: %s fallback to scheme indent" err)
     (scheme-indent-function indent-point state))))

(defun gosh-smart-indent-1 (indent-point state)
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
             (fnsym (intern-soft function))
             (method (and (not (assq fnsym (gosh-extract-local-vars)))
                          (get fnsym 'scheme-indent-function)))
             (importers (gosh-extract-importers
                         (gosh-parse-read-all)))
             indent)
        (cond
         ((setq indent (gosh--smart-indent-assoc-symbol fnsym function importers))
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

ALIST     ::= { MODULE | PROCEDURE } ;
MODULE    ::= MODULE-SYMBOL , { PROCEDURE } ;
PROCEDURE ::= REGEXP , LEVEL | PROCEDURE-SYMBOL , LEVEL ;

LEVEL            ::= number ;
REGEXP           ::= string ;
MODULE-SYMBOL    ::= symbol ;
PROCEDURE-SYMBOL ::= symbol ;
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

(defun gosh--smart-indent-assoc-symbol (symbol name &optional importers alist)
  (let ((module (intern (gosh-parse-current-module)))
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
          ;;TODO ???? in-module `t' ????
          (dolist (importer importers)
            (let* ((mod (nth 0 importer))
                   (prefix (nth 1 importer))
                   (res
                    (cond
                     ((null importer))
                     ((eq module (car x))
                      (gosh--smart-indent-assoc-symbol symbol name importers (cdr x)))
                     ((not (eq mod (car x))) nil)
                     ((string= prefix "")
                      (gosh--smart-indent-assoc-symbol symbol name importers (cdr x)))
                     ((gosh-string-prefix-p prefix name)
                      (let* ((name2 (substring name (length prefix)))
                             (symbol2 (intern-soft name2)))
                        (gosh--smart-indent-assoc-symbol
                         symbol2 name2 importers (cdr x))))
                     (t nil))))
              (when res
                (throw 'found res)))))))
      ;; not found
      nil)))


;;;
;;; Smart import
;;;

;; -> ALIST (same as `gosh-info-doc--env')
(defun gosh-smart-import--exports-in-path (directory)
  (mapcar
   (lambda (file)
     (cons (intern (gosh-file->module file))
           (mapcar
            (lambda (sym)
              (cons sym nil))
            (gosh-env-exports-functions file))))
   (and (file-directory-p directory)
        (directory-files-recursively directory "\\.scm\\'"))))

(defun gosh-smart-import--search-in-path (path symbol)
  (let ((alist (gosh-smart-import--exports-in-path path)))
    (gosh-smart-import-guess-module alist symbol)))

(defun gosh-smart-import-guess-module (alist symbol)
  (cl-loop for e in alist
           if (and (assq symbol (cdr-safe e))
                   ;; first element may be module.
                   (gosh-symbol-p (car-safe e)))
           return (gosh-symbol-name (car-safe e))))

(defun gosh-smart-import-search-module (sym force-all)
  (catch 'found
    ;; search imported modules
    (let ((module (gosh-smart-import-guess-module gosh-info-doc--env sym)))
      (when module
        (throw 'found module))
      (dolist (index gosh-index-user-cache)
        (let ((env (gosh-index--create-env index)))
          (setq module (gosh-smart-import-guess-module env sym))
          (when module
            (throw 'found module))))
      (when force-all
        (dolist (path (gosh-load-path))
          (gosh--temp-message "Searching on %s" path)
          (setq module (gosh-smart-import--search-in-path path sym))
          (when module
            (throw 'found module)))))
    (error "Module for `%s' is not found" sym)))


;;;
;;; info integration
;;;

;; MODULE-ALIST: (MODULE<symbol> . SYMBOL-ALIST)
;; SYMBOL-ALIST: ((SYMBOL [SIGNATURE]) ...)
;; SIGNATURE: TODO descirbe more
(defvar gosh-info-doc--env nil)

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
  (cl-loop with documented = (gosh-info--documented-modules)
           for m in (gosh-available-modules)
           unless (member m documented)
           collect m))

(defun gosh-info--documented-modules ()
  (cl-loop for e in gosh-info-doc--env
           if (and (car-safe e)
                   (symbolp (car-safe e)))
           collect (symbol-name (car-safe e))))

(defun gosh-info-doc-initialize ()
  (setq gosh-info-doc--env
        (gosh-info-doc--generate-signatures)))

;; IMPORTERS : `t` or alist of (MODULE . PREFIX)
(defun gosh-info-doc-env (importers)
  (gosh-append-map
   (lambda (e)
     (let ((maybe-module (car-safe e)))
       (cond
        ((symbolp maybe-module)
         (and-let*
             ((prefix (or
                       (and (eq importers t) "")
                       (let ((importer (assq maybe-module importers)))
                         (nth 1 importer))))
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

(defun gosh-info-doc--get-filename ()
  ;; `Info-find-file' break current window settings.
  (save-window-excursion
    (let ((file (ignore-errors
                  (Info-find-file "gauche-refe"))))
      (unless file
        (message "Gauche info file is not installed?\
 Otherwise set `Info-additional-directory-list' correctly."))
      file)))

(defun gosh-info-doc--generate-signatures ()
  (and-let*
      ((infofile (gosh-info-doc--get-filename))
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
            (cond
             ((null signature))
             ((gosh-symbol-p signature)
              (when in-module
                (setq res (cons (nreverse in-module) res)))
              (setq in-module (cons signature nil)))
             (t
              (cond
               ;; expand special lambda ^a - ^z
               ((eq (car-safe signature) '^c)
                (setq in-module
                      (append
                       (cl-loop for c downfrom ?z downto ?a
                                collect (let ((sym (intern (format "^%c" c))))
                                          (cons sym (cdr-safe signature))))
                       in-module)))
               (t
                (setq in-module (cons signature in-module))))))))))
    (when in-module
      (setq res (cons (nreverse in-module) res)))
    (nreverse res)))

(defun gosh-info-doc--text-to-signature (category text)
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
       ;; (logger-debug "%s: %s" category text)
       nil))
    (cond
     ((or (null res)
          ;; exclude (setter ~) like form
          (not (gosh-symbol-p (car res)))) nil)
     ((member category '("function" "method" "generic function"))
      `(,(car-safe res) (lambda ,(cdr-safe res))))
     ((member category '("macro" "special form"))
      `(,(car-safe res) (syntax ,(cdr-safe res))))
     ((member category '("class" "condition type" "builtin class"))
      ;; slots and super is `nil'
      `(,(car-safe res) (class nil nil)))
     ((member category '("ec qualifier"))
      ;;TODO
      nil)
     ((member category '("module"))
      (car-safe res))
     ((member category '("constant" "variable" "parameter"))
      `(,(car-safe res) (,(intern category))))
     ((member category '("program clause" "command option"
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

(defun gosh-info-show-index (symbol-name)
  "Popup `info' buffer"
  (interactive
   (let ((sym (gosh-parse-symbol-at-point)))
     (list (symbol-name sym))))
  (let (message-log-max)
    (info-lookup-symbol symbol-name)))


;;;
;;; test functions
;;;

(defface gosh-test-errline
  '((t :inherit error))
  "Face used for marking error lines."
  :group 'gosh-mode)

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
  (cl-loop with res = nil
           for (msg show) in gosh-test--message-alist
           do (cl-loop for err in errors
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
    (cl-loop for (msg gsym lsym) in (gosh-test--parse-results errors)
             do
             (let ((greg (regexp-quote gsym))
                   (lreg (and lsym (regexp-quote lsym))))
               ;; search backward. last definition is the test result. maybe...
               (goto-char (point-max))
               (when (re-search-backward (format "^(def.+?\\_<\\(%s\\)\\_>" greg) nil t)
                 (let ((beg (match-beginning 1))
                       (fin (match-end 1)))
                   (when lreg
                     (catch 'done
                       (while (re-search-forward (format "\\_<\\(%s\\)\\_>" lreg) nil t)
                         (when (gosh-context-code-p)
                           (setq beg (match-beginning 1))
                           (setq fin (match-end 1))
                           (throw 'done t)))))
                   (gosh-test--make-error beg fin msg)))))))

(defun gosh-test--make-error (beg end tooltip-text)
  "Allocate a error marker in range BEG and END."
  (unless (gosh-test--region-has-mark-p beg end)
    (gosh-test--mark beg end tooltip-text)))

(defun gosh-test--update-modeline ()
  ;; ignore all errors.
  ;; this function invoke from timer or after-save-buffer hook
  (condition-case err
      (progn
        (let* ((prev-module (gosh-mode-get :tested-module))
               (module (gosh-test--update-module)))
          (when (or (gosh-test--buffer-was-changed-p)
                    (not (equal prev-module module)))
            (gosh-test--reset-result)))
        (force-mode-line-update))
    (error
     (message "%s" err))))

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
    '(:propertize
      "Unknown"
      face gosh-modeline-lightdown-face)))

(defun gosh-test--reset-result ()
  (gosh-mode-put :modeline-tested-module nil)
  (gosh-mode-put :modeline-test-result
    '(:propertize
      "Unknown"
      face gosh-modeline-lightdown-face)))

;; Execute subprocess that have sentinel and filter
;;  subprocess load current file and evaluate `(test-module some-module)'
;;  -> sentinel read test-module result
(defun gosh-test--invoke-process (module)
  (let ((test-buffer (current-buffer))
        (file (gosh-mode--maybe-temp-file))
        (result-buf (generate-new-buffer " *gosh-mode-test* "))
        proc)
    ;; clear overlay
    (gosh-test--clear-errors)
    (gosh-mode-put :modeline-test-result
      `(:propertize
        "Checking"
        face gosh-modeline-working-face))
    (gosh-mode-put :modeline-load-status
      '(:propertize
        "Checking"
        face gosh-modeline-working-face))
    (gosh-mode-put :test-time (float-time))
    (setq proc (gosh-start-process
                "Gosh test" result-buf
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
                  `(:propertize
                    "Invalid"
                    face gosh-modeline-error-face
                    help-echo ,(concat "Gosh error: " message)
                    mouse-face mode-line-highlight)))))
           (t
            (with-current-buffer buffer
              (gosh-mode-put :modeline-load-status
                '(:propertize
                  "Valid"
                  face
                  gosh-modeline-normal-face
                  help-echo
                  "Gosh test: This is loadable file"
                  mouse-face
                  mode-line-highlight))))))))))

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
         text))))))

(defun gosh-test--clear-errors ()
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (gosh-test--mark-p ov)
        (delete-overlay ov)))))

(defun gosh-test--mark (start end tooltip-text)
  (let ((ov (make-overlay start end nil t t)))
    (overlay-put ov 'face 'gosh-test-errline)
    (overlay-put ov 'help-echo tooltip-text)
    (overlay-put ov 'gosh-test-mark t)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'evaporate t)
    ;;TODO what is fringe
    ;; (overlay-put ov 'before-string 'fringe)
    ov))

(defun gosh-test--region-has-mark-p (start end)
  (cl-loop for ov in (overlays-in start end)
           when (gosh-test--mark-p ov)
           return t))

(defun gosh-test--mark-p (obj)
  (and (overlayp obj)
       (overlay-get obj 'gosh-test-mark)))

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
                        `(:propertize
                          "OK"
                          face gosh-modeline-normal-face
                          help-echo
                          "Gosh test: test-module cannot detect error"
                          mouse-face mode-line-highlight)))
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

(defcustom gosh-test-module-confirm t
  "Confirm before \\[gosh-test-module].
`gosh-test-module' evaluate current buffer then call test-module. So, this function
potentially has security risk. To notify the user about this risk, show confirm prompt."
  :group 'gosh-mode
  :type 'gosh-mode)

(defun gosh-test-module ()
  "Execute test for module which is cursor indicated to.
Set `gosh-test-module-confirm' nil if you do not want confirm prompt."
  (interactive)
  (let ((module (gosh-test--update-module)))
    (unless (or (not gosh-test-module-confirm)
                (y-or-n-p "Do not test malicious code."))
      (signal 'quit nil))
    (when gosh-test-module-confirm
      (message "Please set `gosh-test-module-confirm' nil if you do not want prompt.")
      (sit-for 0.5))
    (gosh-test--invoke-process module)))

(defun gosh-test-print-result-at-point ()
  "Print test result if there is."
  (interactive)
  (cond
   ((null (gosh-mode-get :modeline-test-result))
    (message "Current module is not tested yet."))
   (t
    (let ((msgs (cl-loop for o in (overlays-at (point))
                         if (gosh-test--mark-p o)
                         collect (overlay-get o 'help-echo))))
      (cond
       (msgs
        (ding)
        (message "Test Error: %s %s"
                 (mapconcat 'identity msgs ", ")
                 (propertize "(This mark possiblly point to the wrong place.)"
                             'face 'font-lock-warning-face)))
       (t
        (message "No error at point.")))))))

(defun gosh-test-view-result ()
  "View all test error results in this buffer."
  (interactive)
  (cond
   ((null (gosh-mode-get :modeline-test-result))
    (message "Current module is not tested yet."))
   (t
    (let ((msg (cadr (memq 'help-echo (gosh-mode-get :modeline-test-result)))))
      (cond
       (msg
        (ding)
        (message "%s" msg))
       (t
        (message "No error.")))))))


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
  (cl-loop with msg = (format
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
  (cl-loop with point = (point)
           with no-confirm
           with done
           with prev-region
           with new = (gosh-refactor--symbol-regexp new-string)
           do
           (let* ((region (gosh-refactor--next-region point))
                  (start (car region))
                  (end (cdr region))
                  (ovs (gosh-refactor--scheduled-overlays start end)))
             (cl-loop initially (progn
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
                              (cl-case query
                                ((yes)
                                 (gosh-refactor--replace-string ov new-string))
                                ((no)
                                 (gosh-refactor--mark-as-finished ov))
                                ((quit)
                                 (setq done t)
                                 (cl-return))
                                ((all-of-buffer)
                                 (gosh-refactor--replace-string ov new-string)
                                 (sit-for 0.2)
                                 (setq start (save-excursion
                                               (gosh-refactor--goto-toplevel)
                                               (point)))
                                 (setq no-confirm query))
                                ((all-of-highlight)
                                 (gosh-refactor--replace-string ov new-string)
                                 (sit-for 0.2)
                                 (setq no-confirm query))))
                          (when (eq (overlay-get ov 'face) 'gosh-refactor-on-cursor-face)
                            (overlay-put ov 'face old-face)))))
             (setq prev-region region)
             ;; move base point to start of region
             (setq point start))
           ;;TODO consider this condition
           never (or done (and
                           (= (car prev-region) (point-min))
                           (= (cdr prev-region) (point-max))))))

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
    (cl-loop for next on (cdr (memq base overlays))
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
    (define-key map "\C-c\M-I" 'gosh-mode-import-module-maybe)
    (define-key map "\C-c\M-A" 'gosh-mode-autoload-module-maybe)
    (define-key map "\C-c\C-j" 'gosh-mode-jump)
    (define-key map "\C-c\C-u" 'gosh-test-module)
    (define-key map "\C-c?" 'gosh-info-show-index)
    (define-key map "\C-c\M-r" 'gosh-refactor-rename-symbol)
    ;; (define-key map "\C-c\er" 'gosh-refactor-rename-symbol-afaiui)
    ;; TODo consider key bindings
    (define-key map "\C-c\C-v" 'gosh-test-view-result)
    (define-key map "\C-c\C-p" 'gosh-test-print-result-at-point)

    (define-key map ")" 'gosh-paren-insert-close)
    (define-key map "]" 'gosh-paren-insert-close-bracket)
    (define-key map "(" 'gosh-paren-insert-open)
    (define-key map "[" 'gosh-paren-insert-open-bracket)

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
  "Major mode for editing Gauche code.
This mode is originated from `scheme-mode' but specialized to edit Gauche code."
  (if (boundp 'syntax-propertize-function)
      (set (make-local-variable 'syntax-propertize-function)
           'gosh-syntax-table-apply-region)
    (set (make-local-variable 'font-lock-syntactic-keywords)
         (append
          font-lock-syntactic-keywords
          gosh-font-lock-syntactic-keywords)))
  (set (make-local-variable 'after-change-functions)
       'gosh-mode--after-change)
  ;;TODO cancel-timer after kill all gosh-mode
  (unless gosh-mode--timer
    (setq gosh-mode--timer
          (run-with-idle-timer gosh-mode--timer-delay t
                               'gosh-mode-watcher)))
  (add-to-list (make-local-variable 'mode-line-process)
               'gosh-mode-line-process
               'append)
  (add-hook 'kill-buffer-hook 'gosh-mode--cleanup-buffer nil t)
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

(defun gosh-mode--after-change (start end old-len)
  ;;check executable
  (when (< end 255)
    (gosh-mode-put :executable nil))
  (gosh-mode-put :modtime (float-time))
  (gosh-test--reset-status))

(defun gosh-mode--maybe-temp-file ()
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

(defvar gosh-mode--previous-buffer nil)
(defvar gosh-mode-switch-buffer-hook nil)

(defun gosh-mode--follow-target-buffer ()
  (when (or (null gosh-mode--previous-buffer)
            (not (eq gosh-mode--previous-buffer (current-buffer))))
    (ignore-errors
      (run-hooks 'gosh-mode-switch-buffer-hook)))
  (setq gosh-mode--previous-buffer
        (and (derived-mode-p 'gosh-mode)
             (current-buffer))))

(add-hook 'window-configuration-change-hook 'gosh-mode--follow-target-buffer)
;;TODO unload
;; (remove-hook 'window-configuration-change-hook 'gosh-mode--follow-target-buffer)

(defvar gosh-mode-cleanup-buffer-hook nil)
(defvar gosh-mode-cleanup-hook nil)

(defun gosh-mode--cleanup-buffer ()
  (let ((file (gosh-mode-get :temp-file)))
    (when (and file (file-exists-p file))
      (ignore-errors
        (delete-file file))))
  (ignore-errors
    (run-hooks 'gosh-mode-cleanup-buffer-hook))
  (remove-hook 'kill-buffer-hook 'gosh-mode--cleanup-buffer t))

(defun gosh-mode--cleanup-all ()
  (ignore-errors
    (dolist (buf (buffer-list))
      (when (eq (buffer-local-value 'major-mode buf) 'gosh-mode)
        (with-current-buffer buf
          (gosh-mode--cleanup-buffer))))

    ;; delegate procedure
    (run-hooks 'gosh-mode-cleanup-hook)))

(defun gosh-mode--insert-use-statement (module)
  (forward-line 0)
  (indent-for-tab-command)
  ;; save cursor at start of import statement
  (save-excursion
    (insert (format "(use %s)\n" module))
    (indent-for-tab-command))
  (message "Module %s is imported now." module)
  (sit-for 0.5))

(defun gosh-mode--import-module (target current-module)
  (save-excursion
    (goto-char (point-min))
    (cond
     ;; only consider `use' statement.
     ((let ((regexp (format "(use +\\_<%s\\_>" target)))
        (re-search-forward regexp nil t))
      (cond
       ((gosh-context-code-p)
        (ding)
        (message "Module %s have already been imported." target))
       ((and (forward-line 0)
             (looking-at "^\\([\s\t]*;+[\s\t]*\\)"))
        ;; uncomment guessed as temporarily commented out statement.
        (replace-match "" nil nil nil 1))
       (t
        ;;TODO
        )))
     ((re-search-forward "^ *\\((use\\_>\\)" nil t) ; first `use' statements
      (gosh-mode--insert-use-statement target))
     ((let* ((qmodule (regexp-quote current-module))
             (regexp (format "^ *(define-module[\s\t]+%s[\s\t\n]" qmodule)))
        (re-search-forward regexp nil t))
      (forward-line 0)
      (gosh-mode--insert-use-statement target))
     ((re-search-forward "^ *(" nil t)  ; first statement
      (gosh-mode--insert-use-statement target))
     (t
      (error "Unable find properly point to insert `use' statement")))))

(defun gosh-mode--autoload-procedure (symbol module)
  (save-excursion
    (let ((init-point (point)))
      (goto-char (point-min))
      (cond
       ((progn
          (goto-char init-point)
          (re-search-backward "^(" nil t))
        (forward-line -1)
        (insert (format "\n(autoload %s %s)\n" module symbol)))
       (t
        (error "Unable find properly point to insert `autoload' statement"))))))

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
        (setcar key (concat "[\[\(]" (substring regexp 1)))))))

(defvar gosh-font-lock--keywords-2
  (let ((keywords (gosh-font-lock--clone-keywords scheme-font-lock-keywords-2)))
    (setq keywords
          (cons
           `(
             ,(concat
               "(\\(define\\*?\\(?:"
               "\\(-constant\\|-inline\\)"
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

;;TODO rename
(defconst gosh-font-lock-syntactic-keywords
  `(
    (,gosh-regexp-literal-regexp
     ;; (15) is generic string delimiter
     (1 (6) t) (2 (15)) (4 (15) nil t) (5 (15) nil t))
    (,gosh-regexp-vector-prefix
     (0 (6)))
    ))

(defun gosh-syntax-table-apply-region (start end)
  (let ((modified (buffer-modified-p)))
    ;;TODO i couldn't understand enough but two time call
    (unwind-protect
        (save-excursion
          (gosh-syntax-table-apply-region-1 start end)
          (gosh-syntax-table-apply-region-1 start end))
      (set-buffer-modified-p modified))))

(defun gosh-syntax-table-apply-region-1 (start end)
  (let ((inhibit-read-only t))
    (goto-char start)
    (while (re-search-forward "#/" end t)
      (let ((beg (match-beginning 0)))
        (when (gosh-context-code-p beg)
          (gosh-syntax-table-set&go-properties beg ?/ ?/))))
    (goto-char start)
    (while (re-search-forward "#\\[" end t)
      (let ((beg (match-beginning 0)))
        (when (gosh-context-code-p beg)
          (gosh-syntax-table-set&go-properties beg ?\[ ?\]))))
    (goto-char start)
    (while (re-search-forward "#[usUS]\\([0-9]+?\\)" end t)
      (gosh-syntax-table-put-property
       (match-beginning 0) (match-end 0) '(6)))))

(defun gosh-syntax-table-put-property (beg end value)
  (put-text-property beg end 'syntax-table value (current-buffer)))

(defun gosh-syntax-table-set&go-properties (beg start-char end-char)
  (let ((curpos beg)
        ;; "aa #/regexp/"
        ;; #/ignore case regexp/i
        ;; #/regexp \/contains slash \//
        (max (point-max))
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
        (when (= (char-after curpos) start-char)
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
         ((= (char-after curpos) end-char)
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
      (setq curpos (+ curpos 1)))
    (goto-char curpos)))

;;
;; jump
;;

(defun gosh-jump-to-localdef (sym)
  (when (assq sym (gosh-extract-local-vars))
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
         (re-search-forward (format "\\_<%s\\_>" sym) nil t)
         ;;TODO ??? why globaldef? here
         (gosh-jump-to-globaldef sym))))))

(defun gosh-jump-to-module-globaldef (module symbol forms)
  (when (assq symbol (gosh-cache-module-global-env module))
    (let* ((mod-file (gosh-module->file module))
           (buf (get-file-buffer (file-truename mod-file))))
      (set-buffer (or buf (gosh--find-file-noselect mod-file)))
      (when (gosh-jump-to-globaldef symbol)
        (switch-to-buffer (current-buffer))
        t))))

(defun gosh-jump-to-globaldef (definition)
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

;;
;; command
;;

(defun gosh-mode-sort-sexp-region (start end)
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
                     (text (buffer-substring-no-properties beg end)))
                (setq reg-end end)
                (setq lis (cons (list key text) lis))))
          (error nil))
        (when reg-end
          (setq lis (sort lis (lambda (x y) (string-lessp (car x) (car y)))))
          (delete-region reg-start reg-end)
          (goto-char reg-start)
          (dolist (s lis)
            (let ((text (cadr s)))
              (when (string-match "\\`[\s\t\n]+" text)
                (setq text (substring text (match-end 0))))
              (insert text)
              (unless (memq (char-before) '(?\n))
                (insert "\n")))))))))

;;TODO FIXME: more sophistecated algorithm
(defun gosh-mode-jump ()
  "Jump to current symbol definition.
"
  (interactive)
  (let* ((sym (gosh-parse-symbol-at-point))
         (symnm (symbol-name sym))
         (forms (gosh-parse-read-all)))
    (catch 'found
      ;; search local variable
      (when (gosh-jump-to-localdef sym)
        (throw 'found t))
      ;; search toplevel definitions
      (when (gosh-jump-to-globaldef sym)
        (throw 'found t))
      ;; search base modules
      (dolist (x (gosh-extract-base-modules forms))
        (when (gosh-jump-to-module-globaldef x sym forms)
          (throw 'found t)))
      ;; search imported modules
      (let ((importers (gosh-extract-importers forms)))
        (cl-loop for (module modprefix) in importers
                 do (and-let* ((mod-file (gosh-module->file module))
                               (prefix-re (concat "\\`" (regexp-quote modprefix)))
                               ;; no prefix match to every symbol "\\`"
                               ((string-match prefix-re symnm))
                               (symnm2 (substring symnm (length modprefix)))
                               (sym2 (intern-soft symnm2)))
                      (when (and mod-file
                                 (memq sym2 (gosh-env-exports-functions mod-file)))
                        (when (gosh-jump-to-module-globaldef module sym2 forms)
                          (throw 'found t))))))
      ;; jump to module (cursor point as a module name)
      (let ((file (gosh-module->file sym)))
        (when file
          (switch-to-buffer (gosh--find-file-noselect file))
          (throw 'found t)))
      (message "Not found definition %s" sym))))

(defun gosh-mode-import-module-maybe (&optional force-all)
  "Import a new module if missing. If FORCE-ALL is non-nil search on all gauche load-path.
FORCE-ALL make slow the Emacs, of course.

if define-module is exists then insert statement to the form of `define-module'
otherwise insert top level of the script."
  (interactive "P")
  (barf-if-buffer-read-only)
  (let* ((sym (gosh-parse-symbol-at-point))
         (cur-module (gosh-parse-current-module))
         module)
    (unless sym
      (error "Here is no symbol to import a module"))
    (setq module (gosh-smart-import-search-module sym force-all))
    (gosh-mode--import-module module cur-module)))

(defun gosh-mode-autoload-module-maybe (&optional force-all)
  "Add autoload if missing. If FORCE-ALL is non-nil search on all gauche load-path.
FORCE-ALL make slow the Emacs, of course.
"
  (interactive "P")
  (barf-if-buffer-read-only)
  (let* ((sym (gosh-parse-symbol-at-point))
         module)
    (unless sym
      (error "Here is no symbol to import a module"))
    (setq module (gosh-smart-import-search-module sym force-all))
    (gosh-mode--autoload-procedure sym module)))


;;;
;;; Eval minor mode
;;;

;; info 6.20.3 section
(defconst gosh-eval-expression-command-format
  (eval-when-compile
    (concat
     "(let1 port (open-output-file \"%s\") "
     "(unwind-protect "
     " (begin (standard-output-port port) (standard-error-port port) "
     " (begin0 (with-output-to-port port (lambda () %s)))) "
     " (close-output-port port)))"
     )))

(defconst gosh-eval-guard-format
  (eval-when-compile
    (concat
     "(guard (e "
     " (else (print \"%s\" "
     " (or "
     " (and (slot-exists? e 'message) (slot-ref e 'message))"
     " \"ERROR\""
     " )))) %s)\n")))

(defconst gosh-eval-unload-user-module-format
  (eval-when-compile
    (concat
     "(begin"
     " (and-let* ((m (find-module 'user))) (hash-table-clear! (module-table m)))"
     " (define-in-module user *program-name* \"%s\")"
     " (define-in-module user *argv* ())"
     ")\n"
     )))

(defvar gosh-eval-mode-map nil)

(let ((map (or gosh-eval-mode-map (make-sparse-keymap))))

  (define-key map "\C-c\eR" 'gosh-eval-reset-process)
  (define-key map "\C-c\C-b" 'gosh-eval-buffer)
  (define-key map "\C-c\C-n" 'gosh-eval-region)
  (define-key map "\C-x\C-e" 'gosh-eval-last-sexp)
  (define-key map "\M-:" 'gosh-eval-expression)
  (define-key map "\M-\C-x" 'gosh-eval-defun)

  (setq gosh-eval-mode-map map))

(define-minor-mode gosh-eval-mode
  "Gosh sticky process mode.
Evaluate s-expression, syntax check, etc."
  :keymap gosh-eval-mode-map
  (if gosh-eval-mode
      (add-hook 'gosh-mode-timer-functions 'gosh-eval-backend-watcher nil t)
    (remove-hook 'gosh-mode-timer-functions 'gosh-eval-backend-watcher t))
  (gosh-eval-backend-switch-context))

(defun gosh-eval-mode-on ()
  (gosh-eval-mode 1))

(defun gosh-eval-mode-off ()
  (gosh-eval-mode -1))

(defun gosh-eval (sexp-string)
  ;;TODO unbaranced parenthese must be error before send.
  (let* ((hash (concat (md5 sexp-string) " ")) ;; hash string is separator.
         (proc (gosh-eval-check-process))
         (output-file (process-get proc 'gosh-eval-output-file))
         (eval-form
          (format gosh-eval-guard-format hash
                  (format
                   gosh-eval-expression-command-format
                   output-file
                   sexp-string)))
         result)
    (setq result (gosh-eval-low-level-action eval-form proc))
    (when (string-match (concat "^" hash "\\(.*\\)") result)
      (signal 'gosh-eval-error (list (match-string 1 result))))
    result))

(defun gosh-eval-get-output ()
  ;; call after executing `gosh-eval'
  (let* ((proc (gosh-eval-check-process))
         (file (process-get proc 'gosh-eval-output-file)))
    ;; ignore huge file. not concern about error.
    (with-temp-buffer
      (let* ((cs (process-coding-system proc))
             (coding-system-for-read (car cs)))
        (insert-file-contents file)
        (buffer-string)))))

(defun gosh-eval-low-level-action (sexp-string &optional process)
  (let* ((proc (or process (gosh-eval-check-process)))
         start end)
    (with-current-buffer (process-buffer proc)
      (gosh-eval-wait-locking proc)
      (process-put proc 'gosh-eval-locking t)
      (unwind-protect
          (progn
            (setq start (point-max))
            (process-send-string proc sexp-string)
            (let ((inhibit-quit t))
              ;; wait output from command
              (while (= start (point-max))
                (gosh-eval-wait proc))
              ;; wait return prompt from process.
              (while (not (gosh-eval-prompt-match))
                (gosh-eval-wait proc))
              ;;remove trailing newline
              (setq end (1- (match-beginning 0))))
            (unless end
              (signal 'quit nil)))
        (process-put proc 'gosh-eval-locking nil))
      (buffer-substring start end))))

(put 'gosh-eval-error 'error-conditions '(gosh-eval-error error))
(put 'gosh-eval-error 'error-message "Gosh error")

(defun gosh-eval-unload-user-module ()
  (gosh-eval-low-level-action
   (format gosh-eval-unload-user-module-format
           gosh-default-command-internal)))

(defun gosh-eval-cleanup ()
  (cl-loop for (_ . proc) in gosh-eval-process-alist
           do (when (process-live-p proc)
                (delete-process proc))))

(add-hook 'gosh-mode-cleanup-hook 'gosh-eval-cleanup)

;;TODO rename
(defun gosh-eval-buffer-switched ()
  ;;TODO require deliberation
  )

(add-hook 'gosh-mode-switch-buffer-hook 'gosh-eval-buffer-switched)

(defvar gosh-eval-suppress-discard-input nil)

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

(defun gosh-eval-wait-locking (proc)
  (while (process-get proc 'gosh-eval-locking)
    (unless gosh-eval-suppress-discard-input
      (discard-input))
    (sleep-for 0.1)))

(defun gosh-eval-wait (proc)
  (sleep-for 0.1)
  (unless gosh-eval-suppress-discard-input
    (discard-input))
  (when quit-flag
    (kill-process proc)
    (setq inhibit-quit nil)
    (signal 'quit nil)))

(defvar gosh-eval-prompt-regexp "^gosh>[\s\t]*\\'")
(defconst gosh-eval-process-buffer-format " *Gosh-mode<%s>* ")
(defvar gosh-eval-process-alist nil)

(defun gosh-eval-prompt-match ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (looking-at gosh-eval-prompt-regexp)))

(defun gosh-eval-process-filter (proc event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert event)))

(defun gosh-eval-process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (let ((file (process-get proc 'gosh-eval-output-file)))
      (when (and file (file-exists-p file))
        (delete-file file)))))

(defun gosh-eval-active-process (&optional command)
  (let* ((command (or command gosh-default-command-internal))
         proc)
    (cond
     ((and (setq proc (cdr (assoc command gosh-eval-process-alist)))
           (eq (process-status proc) 'run))
      proc)
     (proc
      (delete-process proc)
      nil)
     (t nil))))

(defun gosh-eval-check-process ()
  (unless gosh-default-command-internal
    (error "Assert"))
  ;;TODO switch by executable?
  (let* ((key gosh-default-command-internal)
         (proc (gosh-eval-active-process key)))
    (unless proc
      (let* ((buffer (gosh-eval--create-process-buffer key))
             (dir default-directory))
        (setq proc (gosh-start-process "Gosh backend" buffer "-i"))
        (with-current-buffer buffer
          (unless (file-directory-p default-directory)
            (cd dir))
          (set-process-filter proc 'gosh-eval-process-filter)
          (set-process-sentinel proc 'gosh-eval-process-sentinel)
          (gosh-set-alist 'gosh-eval-process-alist key proc)
          ;; wait for first prompt
          (while (not (gosh-eval-prompt-match))
            (sleep-for 0.1)))))
    (let ((file (process-get proc 'gosh-eval-output-file)))
      (unless (and file (file-exists-p file))
        ;;TODO cleanup tempfile
        (setq file (make-temp-file "gosh-mode-output-"))
        (process-put proc 'gosh-eval-output-file file)))
    proc))

(defun gosh-eval--create-process-buffer (command)
  (let ((buffer (get-buffer-create (format gosh-eval-process-buffer-format command))))
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
(defun gosh-eval-backend-switch-context ()
  (let* ((proc (gosh-eval-check-process))
         (file (gosh-mode--maybe-temp-file))
         (directory (expand-file-name default-directory)))
    (unless (file-directory-p directory)
      (error "Directory is not exist"))
    ;; filter is `gosh-eval-switch-context-filter' means now switching context
    (when (and (not (eq (process-filter proc) 'gosh-eval-switch-context-filter))
               (or (not (eq (process-get proc 'gosh-eval-current-buffer)
                            (current-buffer)))
                   (null (process-get proc 'gosh-eval-switched-time))
                   (null (gosh-mode-get :modtime))
                   (>= (gosh-mode-get :modtime)
                       (process-get proc 'gosh-eval-switched-time))))
      (set-process-filter proc 'gosh-eval-switch-context-filter)
      (process-put proc 'gosh-eval-current-buffer (current-buffer))
      (process-put proc 'gosh-eval-switched-time (float-time))
      (process-put proc 'gosh-eval-locking t)
      (process-send-string proc (format "(sys-chdir \"%s\")\n" directory))
      proc)))

(defun gosh-eval-backend-chdir ()
  (let* ((edir (expand-file-name default-directory))
         (gdir (funcall gosh-default-path-e2g edir)))
    (unless (file-directory-p edir)
      (error "Directory is not exist"))
    (gosh-eval-low-level-action (format "(sys-chdir \"%s\")\n" gdir))))

(defun gosh-eval-switch-context-filter (proc event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert event)
    (when (gosh-eval-prompt-match)
      ;; restore filter and unlock buffer to be enable evaluate expression
      (set-process-filter proc 'gosh-eval-process-filter)
      (process-put proc 'gosh-eval-locking nil))))

(defvar gosh-read-expression-history nil)

(defun gosh-eval-expression-1 (eval-expression-arg &optional suppress-message)
  (let ((module (gosh-parse-current-module))
        form)
    (setq form (format "(with-module %s %s)"
                       module eval-expression-arg))
    (let (result output)
      (condition-case err
          (progn
            (setq result (gosh-eval form))
            (setq output (gosh-eval-get-output)))
        (gosh-eval-error
         (setq output (gosh-eval-get-output))
         ;;emulate emacs error..
         (message "%s" output)
         (signal 'gosh-eval-error (cdr err))))
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
      (let ((result (gosh-eval-low-level-action sexp)))
        ;; FIXME toplevel form error message probablly contains newline...
        (when (string-match "\n" result)
          (signal 'gosh-eval-error (list (format "%s" result))))
        (message "%s" result)))))

(defun gosh-eval--send-region (start end)
  (save-excursion
    ;; evaluate at end module context <- TODO ???? what module mean?
    (goto-char end)
    (gosh-eval-expression (buffer-substring start end))))

(defun gosh-eval--check-backend ()
  (unless gosh-eval-mode
    (error "Command disabled when `gosh-eval-mode' is disabled"))
  (if (gosh-eval-active-process)
      ;; backend already activated
      (gosh-eval-backend-chdir)
    ;; backend deactivated then activate and load current file.
    ;; this case block several seconds in `gosh-eval'
    (gosh-eval-backend-switch-context)))

(defun gosh-eval-backend-watcher ()
  (unless (gosh-eval-active-process)
    (gosh-eval-backend-switch-context)))

(defun gosh-eval-reset-process ()
  "Reset gosh process if exists."
  (interactive)
  (gosh--processing-message "Reset backend process..."
    (let ((active (gosh-eval-active-process)))
      (when active
        (delete-process active))
      (gosh-eval--check-backend))))

(defun gosh-eval-last-sexp ()
  "Send the previous sexp to the sticky backend process.
That sexp evaluated at current module. The module may not be loaded.
Execute \\[gosh-eval-buffer] if you certain the buffer is a reliable code."
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
And print value in the echo area."
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
    (let ((file (gosh-mode--maybe-temp-file)))
      (gosh-eval (format "(load \"%s\")\n" file)))))

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
            (gosh-eval (format "(load \"%s\")\n" file)))
        (delete-file file)))))


;;;
;;; inferior mode
;;;

;; base/super module definitions + import module exported definitions
;; string-copy: to resolve shared structure.
(defconst gosh-inferior--imports-command-format
  (eval-when-compile
    (concat
     "(apply append "
     "(hash-table-map (module-table (current-module)) "
     " (lambda (sym gloc) (string-copy (symbol->string sym)))) "
     "(map (lambda (mod) (map (lambda (sym) "
     " (string-copy (symbol->string sym))) (module-exports mod))) "
     " (module-imports (current-module))))\n")))

(defvar gosh-inferior-mode-map nil)

(unless gosh-inferior-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\M-\C-i" 'gosh-smart-complete)
    (define-key map "\C-c?" 'gosh-info-show-index)
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
  (setq mode-name "Inferior Gosh")
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
     (gosh-inferior--import-candidates proc))))

(defun gosh-inferior--import-candidates (proc)
  "Gather symbols from PROC current-module context"
  (gosh-snatch-process-candidates proc gosh-inferior--imports-command-format))

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
    (gosh-complete-expand sym cands)))

;;
;; Snatch filter (in gosh-inferior-mode)
;;

(defvar gosh-snatch--prompt-regexp "\ngosh>[\s\t]*$")
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
        (when (string-match gosh-snatch--prompt-regexp gosh-snatch--filter-string-stack)
          ;; read scheme '() to nil but to finish up the filter change to `t'
          (setq gosh-snatch--filter-candidates
                (or
                 (read gosh-snatch--filter-string-stack)
                 t))))
    (error
     ;; step through if error.
     (setq gosh-snatch--filter-candidates t))))


;;;
;;; advice
;;;

(defadvice scheme-mode
    (after gosh-hack-scheme-mode () activate)
  (gosh-mode-maybe-from-scheme-mode))

(defun gosh-deactivate-advice ()
  (ad-disable-advice 'scheme-mode 'after 'gosh-hack-scheme-mode)
  (ad-update 'scheme-mode))

(add-hook 'gosh-mode-cleanup-hook 'gosh-deactivate-advice)



;; Execute when loading.
(defun gosh-initialize ()
  (gosh-default-initialize)
  (gosh-ac-initialize)
  (gosh-info-initialize))

;; called when `unload-feature'
(defun gosh-mode-unload-function ()
  (remove-hook 'kill-emacs-hook 'gosh-mode--cleanup-all)
  (gosh-mode--cleanup-all)
  ;; explicitly return nil
  nil)

(add-hook 'kill-emacs-hook 'gosh-mode--cleanup-all)



(provide 'gosh-mode)

;;; gosh-mode.el ends here
