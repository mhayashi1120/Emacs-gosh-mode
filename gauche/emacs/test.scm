(use gauche.test)

(test-start "extension srfi modules")

(test-section "emacs.gosh-mode")
(use emacs.gosh-mode)
(test-module 'emacs.gosh-mode)
