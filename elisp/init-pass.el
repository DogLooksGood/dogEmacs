;;; -*- lexical-binding: t -*-

(straight-use-package 'pass)

(+pdump-packages 'pass)

(setq pass-username-fallback-on-filename t
      pass-show-keybindings nil)

(autoload #'pass "pass" nil t)

(provide 'init-pass)
;; init-pass.el ends here
