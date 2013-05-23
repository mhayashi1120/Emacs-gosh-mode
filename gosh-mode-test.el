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
  (should-error (gosh-read-just-first "") :type 'end-of-file)
  ;; bracket same as list (Not like a Emacs vector)
  (should (equal (gosh-read-just-first "[1 2]") '(1 2)))
  (should (equal (gosh-read-just-first "(1 2 . 3)") '(1 2 . 3)))
  (should-error (gosh-read-just-first "(1 2 . 3 . )") :type 'invalid-read-syntax)
  (should-error (gosh-read-just-first "(.)") :type 'invalid-read-syntax)

  ;;; string
  (should (equal (gosh-read-just-first "\"a\"") "a"))
  (should (equal (gosh-read-just-first "\"\"") ""))
  (should (equal (gosh-read-just-first "#`\"\"") ""))
  (should (equal (gosh-read-just-first "#`\",(abcd)\"") ",(abcd)"))

  ;;; symbol
  (should (equal (gosh-read-just-first "a") 'a))
  (should (equal (gosh-read-just-first "#:a") [uninterned-symbol "a"]))

  ;;; number
  (should (equal (gosh-read-just-first "1") 1))
  ;; overflow Emacs integer
  (should (equal (gosh-read-just-first "100000000000000000000") [number "100000000000000000000"]))

  ;;; vector
  (should (equal (gosh-read-just-first "#(a b)") [vector [a b]]))
  (should (equal (gosh-read-just-first "#u8(1 2)") [u8vector [1 2]]))
  (should (equal (gosh-read-just-first "#u8(a)") [u8vector [a]]))
  (should (equal (gosh-read-just-first "#u8()") [u8vector []]))

  ;; modifing reader behavior
  (should (equal (gosh-read-just-first "#!fold-caseABCD A") 'A))
  (should (equal (gosh-read-just-first "#!fold-case ABCD") 'abcd))
  (should (equal (with-temp-buffer
                   (insert "#!fold-case ABCD #!no-fold-case ABCD")
                   (goto-char (point-min))
                   (list (gosh-read) (gosh-read)))
                 '(abcd ABCD)))
  (should (equal (with-temp-buffer
                   (insert "#!fold-case ABCD ABCD")
                   (goto-char (point-min))
                   (list (gosh-read) (gosh-read)))
                 '(abcd abcd)))

  ;; test/srfi.scm nested comment
  (should (equal (gosh-read-just-first "#|##|###|#|||#### ||#|||||#|#1") 1))

  ;;; char
  (should (equal (gosh-read-just-first "#\\newline") [char 10]))

  ;;; bool
  (should (equal (gosh-read-just-first "#t") '\#t))
  (should (equal (gosh-read-just-first "#f") '\#f))

  ;;; charset
  (should (equal (gosh-read-just-first "#[1]") [charset "1"]))
  (should (equal (gosh-read-just-first "#[b[:alpha:]a]") [charset "b[:alpha:]a"]))
  (should (equal (gosh-read-just-first "#[[:graph:]]") [charset "[:graph:]"]))
  
  (should (equal (gosh-read-just-first "#[]") [charset ""]))

  ;;TODO
  ;; (should-error (gosh-read-just-first "(1 .  )") :type 'invalid-read-syntax)
  (should (equal (gosh-read-just-first "\"hoge\"") "hoge"))
  (should (equal (gosh-read-just-first "'(1 2 3)") '(quote (1 2 3))))
  (should (equal (gosh-read-just-first "`(1 2)") '(quote (1 2))))
  (should (equal (gosh-read-just-first ",(1 2)") [unquote (1 2)]))

  (should (equal (gosh-read-just-first "(#3=(1 2) #3#)") '((1 2) [back-reference 3])))
  (should (equal (gosh-read-just-first "#,(1 2)") '[reader-constructor (1 2)]))
  
  ;;TODO what should i do?
  (should (equal (gosh-read-just-first "||")  '##))


  ;;TODO consider greedy regexp search testing
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
      (should (equal (gosh-parse--current-context-for-opening) `(let *)))
      (funcall parenthese 3)
      (should (equal (gosh-parse--current-context-for-opening) `(let (*)))))
    (with-temp-buffer
      (insert "(case a () ())")
      (funcall parenthese 2)
      (should (equal (gosh-parse--current-context-for-opening) `(case a *)))
      (funcall parenthese 3)
      ;;TODO () is '() in scheme world..
      (should (equal (gosh-parse--current-context-for-opening) `(case a nil *))))
    (with-temp-buffer
      (insert "(case a [(a)] )")
      (funcall parenthese 2)
      (should (equal (gosh-parse--current-context-for-opening) `(case a *)))
      (forward-char)
      (should (equal (gosh-parse--current-context-for-opening) `(case a (*)))))))

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

(defun gosh-mode-test-BoL (case)
  (let* ((data (split-string case " -> "))
         (source (car data))
         (result (string-match "_" (cadr data))))
    (with-temp-buffer
      (gosh-mode)
      (insert source)
      (goto-char (point-min))
      (re-search-forward "_")
      (replace-match "")
      (gosh-beginning-of-list)
      (should (= (1- (point)) result)))))

(ert-deftest gosh-mode-test--BoL ()
  :tags '(gosh-mode)
  (gosh-mode-test-BoL "(let()_  -> _(let()")
  (gosh-mode-test-BoL "(let(_)  -> (let_()")
  (gosh-mode-test-BoL "(let_()  -> _(let()")
  (gosh-mode-test-BoL "(l_et()  -> _(let()")
  (gosh-mode-test-BoL "(_let()  -> _(let()")
  (gosh-mode-test-BoL "( let _()  -> _(let()")
  (gosh-mode-test-BoL "(func \"AA(_)\"  -> _(func \"AA()\"")
  (gosh-mode-test-BoL "(func #`\"AA(_)\" -> _(func #`\"AA()\"")
  )

(defun gosh-mode-test-funcall-in-text (func text)
  (with-temp-buffer
    (gosh-mode)
    (insert text)
    (goto-char (point-min))
    (re-search-forward "_")
    (replace-match "")
    (funcall func)))

(ert-deftest gosh-mode-test--eldoc-context ()
  :tags '(gosh-mode)
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-sexp-in-list "(_fn \"A\")") '((fn) 0)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-sexp-in-list "(f_n \"A\")") '((fn) 0)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-sexp-in-list "(fn_ \"A\")") '((fn) 0)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-sexp-in-list "(fn _\"A\")") '((fn "A") 1)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-sexp-in-list "(fn \"_A\")") '((fn "A") 1)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-sexp-in-list "(fn \"A_\")") '((fn "A") 1)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-sexp-in-list "(fn \"A\"_)") '((fn "A") 1))))

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
                   (gosh-parse--current-exports))
                 '(hoge  hoge-rest hoge-opt hoge-key hoge-key-opt)))
  (should (equal
           (gosh-mode-test-with  gosh-mode-test-code1
             (gosh-parse--current-globals))
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
