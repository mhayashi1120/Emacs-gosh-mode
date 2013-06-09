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
  (should-error (gosh-read-first-from-string "") :type 'end-of-file)
  ;; bracket same as list (Not like a Emacs vector)
  (should (equal (gosh-read-first-from-string "[1 2]") '(1 2)))
  (should (equal (gosh-read-first-from-string "(1 2 . 3)") '(1 2 . 3)))
  (should-error (gosh-read-first-from-string "(1 2 . 3 . )") :type 'invalid-read-syntax)
  (should-error (gosh-read-first-from-string "(.)") :type 'invalid-read-syntax)

  ;;; string
  (should (equal (gosh-read-first-from-string "\"a\"") "a"))
  (should (equal (gosh-read-first-from-string "\"\"") ""))
  (should (equal (gosh-read-first-from-string "#`\"\"") ""))
  (should (equal (gosh-read-first-from-string "#`\",(abcd)\"") ",(abcd)"))

  ;;; symbol
  (should (equal (gosh-read-first-from-string "a") 'a))
  (should (equal (gosh-read-first-from-string "#:a") [uninterned-symbol "a"]))
  (should (equal (gosh-read-first-from-string "nil") [symbol "nil"]))

  ;;; number
  (should (equal (gosh-read-first-from-string "1") 1))
  ;; overflow Emacs integer
  (should (equal (gosh-read-first-from-string "100000000000000000000") [number "100000000000000000000"]))

  ;;; vector
  (should (equal (gosh-read-first-from-string "#(a b)") [vector [a b]]))
  (should (equal (gosh-read-first-from-string "#u8(1 2)") [u8vector [1 2]]))
  (should (equal (gosh-read-first-from-string "#u8(a)") [u8vector [a]]))
  (should (equal (gosh-read-first-from-string "#u8()") [u8vector []]))

  ;; modifing reader behavior
  (should (equal (gosh-read-first-from-string "#!fold-caseABCD A") 'A))
  (should (equal (gosh-read-first-from-string "#!fold-case ABCD") 'abcd))
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
  (should (equal (gosh-read-first-from-string "#|##|###|#|||#### ||#|||||#|#1") 1))

  ;;; char
  (should (equal (gosh-read-first-from-string "#\\newline") [char 10]))

  ;;; bool
  (should (equal (gosh-read-first-from-string "#t") '\#t))
  (should (equal (gosh-read-first-from-string "#f") '\#f))

  ;;; charset
  (should (equal (gosh-read-first-from-string "#[1]") [charset "1"]))
  (should (equal (gosh-read-first-from-string "#[b[:alpha:]a]") [charset "b[:alpha:]a"]))
  (should (equal (gosh-read-first-from-string "#[[:graph:]]") [charset "[:graph:]"]))
  
  (should (equal (gosh-read-first-from-string "#[]") [charset ""]))

  ;;TODO
  ;; (should-error (gosh-read-first-from-string "(1 .  )") :type 'invalid-read-syntax)
  (should (equal (gosh-read-first-from-string "\"hoge\"") "hoge"))
  (should (equal (gosh-read-first-from-string "'(1 2 3)") '(quote (1 2 3))))
  (should (equal (gosh-read-first-from-string "`(1 2)") '(quote (1 2))))
  (should (equal (gosh-read-first-from-string ",(1 2)") [unquote (1 2)]))

  (should (equal (gosh-read-first-from-string "(#3=(1 2) #3#)") '((1 2) [back-reference 3])))
  (should (equal (gosh-read-first-from-string "#,(1 2)") '[reader-constructor (1 2)]))
  
  ;;TODO what should i do?
  (should (equal (gosh-read-first-from-string "||")  '##))

  ;; http://blog.practical-scheme.net/gauche/20130522-r7rs
  (should (equal (gosh-read-all-from-string "abc'def") '(abc (quote def))))

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
      (should (equal (gosh-paren--next-context) `(let *)))
      (funcall parenthese 3)
      (should (equal (gosh-paren--next-context) `(let (*)))))
    (with-temp-buffer
      (insert "(case a () ())")
      (funcall parenthese 2)
      (should (equal (gosh-paren--next-context) `(case a *)))
      (funcall parenthese 3)
      ;;TODO () is '() in scheme world..
      (should (equal (gosh-paren--next-context) `(case a nil *))))
    (with-temp-buffer
      (insert "(case a [(a)] )")
      (funcall parenthese 2)
      (should (equal (gosh-paren--next-context) `(case a *)))
      (forward-char)
      (should (equal (gosh-paren--next-context) `(case a (*)))))))

(ert-deftest gosh-mode-test--with-bracket ()
  :tags '(gosh-mode)

  (let ((gosh-opening--auto-bracket-alist
         '((let (*))
           (let gosh-symbol-p (*))
           (guard (gosh-symbol-p *))
           )))
    (should (equal (gosh-paren--context-bracket-p '(let loop ((*)))) nil))
    (should (equal (gosh-paren--context-bracket-p '(let loop (*))) t))
    (should (equal (gosh-paren--context-bracket-p '(let (*))) t))
    (should (equal (gosh-paren--context-bracket-p '(let ((*)))) nil))
    (should (equal (gosh-paren--context-bracket-p '(let loop ((a "")) *)) nil))
    (should (equal (gosh-paren--context-bracket-p '(guard (e *))) t))
    (should (equal (gosh-paren--context-bracket-p '(guard (e (else *)))) nil))
    (should (equal (gosh-paren--context-bracket-p '(guard (e (any proc) (else *)))) nil))
    (should (equal (gosh-paren--context-bracket-p '(guard (e (any proc) *))) t))))

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

(defun gosh-mode-test-funcall-in-sexp (func sexp)
  (gosh-mode-test-funcall-in-text func (prin1-to-string sexp)))

(defun gosh-mode-test-parse-local-vars (sexp result)
  (should 
   (equal
    (gosh-mode-test-funcall-in-sexp 'gosh-extract-local-vars sexp)
    result)))

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
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(_fn \"A\")") '((fn) 0)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(f_n \"A\")") '((fn) 0)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(fn_ \"A\")") '((fn) 0)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(fn _\"A\")") '((fn "A") 1)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(fn \"_A\")") '((fn "A") 1)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(fn \"A_\")") '((fn "A") 1)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(fn \"A\"_)") '((fn "A") 1)))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse--current-fnsexp-in-list "(fn \"A\")_") '(nil 0)))
  )

(ert-deftest gosh-mode-test--local-var-detection ()
  :tags '(gosh-mode)
  (gosh-mode-test-parse-local-vars '(define (method a1) _) '((a1)))
  (gosh-mode-test-parse-local-vars '(define (method _ a1)) '())
  (gosh-mode-test-parse-local-vars '(define (method) _) '())
  (gosh-mode-test-parse-local-vars '(define (method) (define (inner a1) _)) '((a1)))
  (gosh-mode-test-parse-local-vars '(define (method a1 a2) (define (inner a1) _)) '((a1) (a2)))
  ;;TODO
  ;; (gosh-mode-test-parse-local-vars '(define (method A1) (define (inner a1)) _) '((A1) (inner (lambda (a1)))))

  (gosh-mode-test-parse-local-vars '(define-method method (A1) _) '((A1)))
  (gosh-mode-test-parse-local-vars '(define-method method ((A1 <integer>)) _) '((A1 <integer>)))

  (gosh-mode-test-parse-local-vars '(define (method a1 :key k1 (k2 1) :rest vars) _) '((a1) (k1) (k2 1) (vars)))
  )


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
  (should (equal (gosh-mode-test-with gosh-mode-test-code1
                   (let ((forms (gosh-parse-read-forms)))
                     (gosh-extract-exports forms)))
                 '(hoge  hoge-rest hoge-opt hoge-key hoge-key-opt)))
  (should (equal
           (gosh-mode-test-with  gosh-mode-test-code1
             (let ((forms (gosh-parse-read-forms)))
               (gosh-extract-globals forms)))
           '((hoge-key-opt (lambda (:optional arg1 (arg2 \#f) :key (key1 \#f) key2)))
             (hoge-key (lambda (:key (key1 \#f) key2)))
             (hoge-opt (lambda (:optional arg1 (arg2 \#f))))
             (hoge-rest (lambda args))
             (hoge (lambda (args))))))
  (should (equal
           (gosh-eldoc--object->string '(hoge (opts (quote ()))))
           "(hoge (opts ()))")))

(ert-deftest gosh-mode-test--parse2 ()
  :tags '(gosh-mode)
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "aa bb_") 'bb))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "aa b_b") 'bb))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "aa _bb") 'bb))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "aa_ bb") 'aa))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "a_a bb") 'aa))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "_aa bb") 'aa))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "_(aa bb)") nil))
  (should (equal (gosh-mode-test-funcall-in-text 'gosh-parse-symbol-at-point "(aa bb)_") nil))
  )

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
