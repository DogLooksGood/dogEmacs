;;; -*- lexical-binding: t; -*-

(straight-use-package 'direnv)

(+pdump-packages 'direnv)

(autoload #'direnv-mode "direnv" nil t)
(autoload #'direnv-allow "direnv" nil t)

(with-eval-after-load "direnv"
  (global-set-key [f9] 'direnv-mode)
  (global-set-key [f10] 'direnv-allow))

(provide 'init-direnv)
