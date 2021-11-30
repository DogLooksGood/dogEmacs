;;; -*- lexical-binding: t -*-

(straight-use-package 'pass)
(straight-use-package 'pinentry)

(setq pass-username-fallback-on-filename t
      pass-show-keybindings nil)

(autoload #'pass "pass" nil t)

(with-eval-after-load "pass"
  (pinentry-start))

(provide 'init-pass)
;; init-pass.el ends here
