;;; gosh-mode-test.el --- Test for gosh-mode

;;; Commentary:
;; 
;; Load this file to execute unit-test.

(require 'el-mock)
(require 'el-expectations)

(defmacro gosh-mode-test-with (code &rest forms)
  `(with-temp-buffer
     (insert ,code)
     ,@forms))

(put 'gosh-mode-test-with 'lisp-indent-function 1)

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

(let ((parenthese 
       (lambda (count)
         (goto-char (point-min))
         (loop repeat count
               do (re-search-forward "(" nil t))
         (backward-char))))
  (with-temp-buffer
    (insert "(let (()) (()))")
    (funcall parenthese 2)
    (assert (equal (gosh-opening--parse-current-context) `(let 0 *)))
    (funcall parenthese 3)
    (assert (equal (gosh-opening--parse-current-context) `(let 0 (*)))))
  (with-temp-buffer
    (insert "(case a () ())")
    (funcall parenthese 2)
    (assert (equal (gosh-opening--parse-current-context) `(case 1 *)))
    (funcall parenthese 3)
    (assert (equal (gosh-opening--parse-current-context) `(case 2 *)))))

(assert (equal (gosh-opening--match-p '(let 1 ((*)))) nil))
(assert (equal (gosh-opening--match-p '(let 1 (*))) t))
(assert (equal (gosh-opening--match-p '(let 0 (*))) t))
(assert (equal (gosh-opening--match-p '(let 0 ((*)))) nil))
(assert (equal (gosh-opening--match-p '(let 2 *)) nil))

;;TODO more test
(dont-compile
  (expectations 
    (desc "stub setup/teardown")
    (expect '(hoge  hoge-rest hoge-opt hoge-key hoge-key-opt)
      (gosh-mode-test-with  gosh-mode-test-code1
        (gosh-parse-current-exports)))
    (expect '((hoge-key-opt (lambda (:optional arg1 (arg2 \#f) :key (key1 \#f) key2)))
              (hoge-key (lambda (:key (key1 \#f) key2)))
              (hoge-opt (lambda (:optional arg1 (arg2 \#f))))
              (hoge-rest (lambda args))
              (hoge (lambda (args))))
      (gosh-mode-test-with  gosh-mode-test-code1
        (gosh-parse-current-globals)))

    (expect "(hoge (opts ()))"
            (gosh-eldoc--object->string '(hoge (opts (quote ())))))

    (when (memq system-type '(windows-nt))
      (expect "c:/cygwin/usr/local/ " (gosh-cygpath->emacs-path "/usr/local"))
      (expect "usr/local/ " (gosh-cygpath->emacs-path "usr/local"))
      (expect "d:/home " (gosh-cygpath->emacs-path "/cygdrive/d/home"))

      (expect "/usr" (gosh-emacs-path->cygpath "c:/cygwin/usr"))
      (expect "/cygdrive/c/usr" (gosh-emacs-path->cygpath "c:/usr"))
      (expect "usr" (gosh-emacs-path->cygpath "usr")))
  ))

(expectations-execute)

(provide 'gosh-mode-test)

;;; gosh-mode-test.el ends here
