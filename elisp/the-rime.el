(use-package liberime-config
  :quelpa (liberime-config
           :fetcher github
           :repo "DogLooksGood/liberime"
           :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))

(require 'rime)

(defun m4d-insert-mode-p ()
  m4d-insert-mode)

(setq rime--enable-predicates '(m4d-insert-mode-p))

(global-set-key (kbd "C-\\") 'rime-toggle)

(add-hook 'm4d-insert-mode-hook 'rime-update-input-method-state)
(add-hook 'm4d-insert-exit-hook 'rime-update-input-method-state)

(provide 'the-rime)
