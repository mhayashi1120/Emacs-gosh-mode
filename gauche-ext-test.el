;;; gauche-ext-test.el --- Gauche mod Test

;;; Commentary:
;; 
;; Load this file to execute unit-test.

(require 'el-mock)
(require 'el-expectations)

(defmacro gauche-ext-test-with (code &rest forms)
  `(with-temp-buffer
     (insert ,code)
     ,@forms))

(put 'gauche-ext-test-with 'lisp-indent-function 1)

(defconst gauche-ext-test-code1
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

;;TODO more test
(dont-compile
  (expectations 
    (desc "stub setup/teardown")
    (expect '(hoge  hoge-rest hoge-opt hoge-key hoge-key-opt)
      (gauche-ext-test-with  gauche-ext-test-code1
	(scm-current-exports)))
    (expect '((hoge-key-opt (lambda (:optional arg1 (arg2 \#f) :key (key1 \#f) key2)))
	      (hoge-key (lambda (:key (key1 \#f) key2)))
	      (hoge-opt (lambda (:optional arg1 (arg2 \#f))))
	      (hoge-rest (lambda args))
	      (hoge (lambda (args))))
      (gauche-ext-test-with  gauche-ext-test-code1
	(scm-current-globals)))

    (expect "(hoge (opts ()))"
	    (scm-object-to-string '(hoge (opts (quote ())))))
  ))

(expectations-execute)

(provide 'gauche-ext-test)

;;; gauche-ext-test.el ends here
