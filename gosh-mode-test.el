;;; gosh-mode-test.el --- Test for gosh-mode

;;; Commentary:
;;
;; Load this file to execute unit-test.

(require 'ert)

(defmacro gosh-mode-test-with (code &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     ,@forms))

(ert-deftest gosh-reader-general-0001 ()
  :tags '(gosh-mode)

  ;;; general
  (should-error (gosh-reader-read-string "") :type 'end-of-file)
  ;; bracket same as list (Not like a Emacs vector)
  (should (equal (gosh-reader-read-string "[1 2]") '((1 2) . 5)))
  (should (equal (gosh-reader-read-string "(1 2 . 3)") '((1 2 . 3) . 9)))
  (should-error (gosh-reader-read-string "(1 2 . 3 . )") :type 'invalid-read-syntax)
  (should-error (gosh-reader-read-string "(.)") :type 'invalid-read-syntax)

  ;;; symbol
  (should (equal (gosh-reader-read-string "a") '(a . 1)))

  ;;; number
  (should (equal (gosh-reader-read-string "1") '(1 . 1)))
  ;; overflow Emacs integer
  (should (equal (gosh-reader-read-string "100000000000000000000") '((number "100000000000000000000") . 21)))

  ;;; vector
  (should (equal (gosh-reader-read-string "#(a b)") '([a b] . 6)))
  (should (equal (gosh-reader-read-string "#u8(1 2)") '((u8 [1 2]) . 8)))
  (should (equal (gosh-reader-read-string "#u8(a)") '((u8 [a]) . 6)))
  (should (equal (gosh-reader-read-string "#u8()") '((u8 []) . 5)))

  ;;TODO u*vector

  ;;; char
  (should (equal (gosh-reader-read-string "#\\newline") '((char 10) . 9)))

  ;;; charset
  (should (equal (gosh-reader-read-string "#[1]") '((charset "1") . 4)))
  (should (equal (gosh-reader-read-string "#[b[:alpha:]a]") '((charset "b[:alpha:]a") . 14)))
  (should (equal (gosh-reader-read-string "#[]") '((charset "") . 3)))

  ;;TODO
  ;; (should-error (gosh-reader-read-string "(1 .  )") :type 'invalid-read-syntax)
  (should (equal (gosh-reader-read-string "\"hoge\"") '("hoge" . 6)))
  (should (equal (gosh-reader-read-string "'(1 2 3)") '((quote (1 2 3)) . 8)))
  (should (equal (gosh-reader-read-string "`(1 2)") '((quote (1 2)) . 6)))
  (should (equal (gosh-reader-read-string ",(1 2)") '((unquote (1 2)) . 6)))
  
  ;;TODO what should i do?
  (should (equal (gosh-reader-read-string "||")  '(## . 2)))
  )

(ert-deftest gosh-mode-test--parse-current-context ()
  :tags '(gosh-mode)

  (let ((parenthese
         (lambda (count)
           (goto-char (point-min))
           (loop repeat count
                 do (re-search-forward "[[(]" nil t))
           (backward-char))))
    (with-temp-buffer
      (insert "(let (()) (()))")
      (funcall parenthese 2)
      (should (equal (gosh-opening--parse-current-context) `(let *)))
      (funcall parenthese 3)
      (should (equal (gosh-opening--parse-current-context) `(let (*)))))
    (with-temp-buffer
      (insert "(case a () ())")
      (funcall parenthese 2)
      (should (equal (gosh-opening--parse-current-context) `(case a *)))
      (funcall parenthese 3)
      ;;TODO () is '() in scheme world..
      (should (equal (gosh-opening--parse-current-context) `(case a nil *))))
    (with-temp-buffer
      (insert "(case a [(a)] )")
      (funcall parenthese 2)
      (should (equal (gosh-opening--parse-current-context) `(case a *)))
      (forward-char)
      (should (equal (gosh-opening--parse-current-context) `(case a (*)))))))

(ert-deftest gosh-mode-test--with-bracket ()
  :tags '(gosh-mode)

  (let ((gosh-opening--auto-bracket-alist
         '((let (*))
           (let gosh-symbol-p (*))
           (guard (gosh-symbol-p *))
           )))
    (should (equal (gosh-opening--context-bracket-p '(let loop ((*)))) nil))
    (should (equal (gosh-opening--context-bracket-p '(let loop (*))) t))
    (should (equal (gosh-opening--context-bracket-p '(let (*))) t))
    (should (equal (gosh-opening--context-bracket-p '(let ((*)))) nil))
    (should (equal (gosh-opening--context-bracket-p '(let loop ((a "")) *)) nil))
    (should (equal (gosh-opening--context-bracket-p '(guard (e *))) t))
    (should (equal (gosh-opening--context-bracket-p '(guard (e (else *)))) nil))
    (should (equal (gosh-opening--context-bracket-p '(guard (e (any proc) (else *)))) nil))
    (should (equal (gosh-opening--context-bracket-p '(guard (e (any proc) *))) t))))

(when (memq system-type '(windows-nt))
  (ert-deftest gosh-mode-test--w32-path ()
    :tags '(gosh-mode)
    (should (equal "c:/cygwin/usr/local/" (gosh-cygpath->emacs-path "/usr/local/")))
    (should (equal "usr/local/" (gosh-cygpath->emacs-path "usr/local/")))
    (should (equal "d:/home" (gosh-cygpath->emacs-path "/cygdrive/d/home")))

    (should (equal "/usr" (gosh-emacs-path->cygpath "c:/cygwin/usr")))
    (should (equal "/cygdrive/c/usr" (gosh-emacs-path->cygpath "c:/usr")))
    (should (equal "usr" (gosh-emacs-path->cygpath "usr")))
    ))


(defconst gosh-mode-test-code1
  "
\(define-module my.test.module
  (export
   hoge hoge-rest
   hoge-opt hoge-key hoge-key-opt))

\(define (hoge args))
\(define (hoge-rest . args))
\(define (hoge-opt :optional arg1 (arg2 #f)))
\(define (hoge-key :key (key1 #f) key2))
\(define (hoge-key-opt :optional arg1 (arg2 #f) :key (key1 #f) key2))
")

(ert-deftest gosh-mode-test--parse1 ()
  :tags '(gosh-mode)
  (should (equal (gosh-mode-test-with  gosh-mode-test-code1
                   (gosh-parse-current-exports))
                 '(hoge  hoge-rest hoge-opt hoge-key hoge-key-opt)))
  (should (equal
           (gosh-mode-test-with  gosh-mode-test-code1
             (gosh-parse-current-globals))
           '((hoge-key-opt (lambda (:optional arg1 (arg2 \#f) :key (key1 \#f) key2)))
             (hoge-key (lambda (:key (key1 \#f) key2)))
             (hoge-opt (lambda (:optional arg1 (arg2 \#f))))
             (hoge-rest (lambda args))
             (hoge (lambda (args))))))
  (should (equal
           (gosh-eldoc--object->string '(hoge (opts (quote ()))))
           "(hoge (opts ()))")))

(ert-deftest gosh-mode-test--defining-indent-rule ()
  :tags '(gosh-mode)
  (let ((gosh--smart-indent-alist nil))
    (should (equal (gosh-smart-indent-rule 'a 1) '(a . 1)))
    (should (equal (gosh-smart-indent-rule 'a 2 'm) '(a . 2)))
    (should (equal (gosh-smart-indent-rule 'a 3) '(a . 3)))
    (should (equal gosh--smart-indent-alist
                   '((m (a . 2))
                     (a . 3))))))

(provide 'gosh-mode-test)

;;; gosh-mode-test.el ends here
