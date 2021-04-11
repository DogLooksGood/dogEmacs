;;; -*- lexical-binding: t; -*-

(straight-use-package 'direnv)

(+pdump-packages 'direnv)

(autoload #'direnv-mode "direnv" nil t)
(autoload #'direnv-allow "direnv" nil t)

(add-hook 'prog-mode-hook #'direnv-mode)

(provide 'init-direnv)
