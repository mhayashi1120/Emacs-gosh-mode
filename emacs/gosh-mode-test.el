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
      (should (equal (gosh-opening--parse-current-context) `(case a [*]))))))

(ert-deftest gosh-mode-test--with-bracket ()
  :tags '(gosh-mode)

  (should (equal (gosh-opening--context-bracket-p '(let loop ((*)))) nil))
  (should (equal (gosh-opening--context-bracket-p '(let loop (*))) t))
  (should (equal (gosh-opening--context-bracket-p '(let (*))) t))
  (should (equal (gosh-opening--context-bracket-p '(let ((*)))) nil))
  (should (equal (gosh-opening--context-bracket-p '(let loop ((a "")) *)) nil)))

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
    (should (equal (gosh-mode-indent-rule 'a 1) '(a . 1)))
    (should (equal (gosh-mode-indent-rule 'a 2 'm) '(a . 2)))
    (should (equal (gosh-mode-indent-rule 'a 3) '(a . 3)))
    (should (equal gosh--smart-indent-alist
                   '((m (a . 2))
                     (a . 3))))))
                     
(provide 'gosh-mode-test)

;;; gosh-mode-test.el ends here
