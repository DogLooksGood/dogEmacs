;;; -*- lexical-binding: t; -*-

(straight-use-package 'direnv)

(setq direnv-use-faces-in-summary nil
      direnv-show-paths-in-summary nil)

(autoload #'direnv-mode "direnv" nil t)
(autoload #'direnv-allow "direnv" nil t)

(add-hook 'prog-mode-hook #'direnv-mode)

(provide 'init-direnv)
