(use-package liberime-config
  :quelpa (liberime-config
           :fetcher github
           :repo "DogLooksGood/liberime"
           :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))

(require 'rime)

(defun m4d-insert-mode-p ()
  m4d-insert-mode)

(setq rime--show-candidate t)

(defun user/rime-not-in-insert-mode ()
  (not m4d-insert-mode))

(defun user/rime-in-quote ()
  (and (equal ?` (char-before)) (equal ?` (char-after))))

(defun user/rime-in-kbd ()
  (and (looking-at "</kbd>")
       (looking-back "<kbd>" 1)))

(setq rime--disable-predicates
      '(rime--after-alphabet-char-p
        rime--prog-in-code-p
        user/rime-not-in-insert-mode
        user/rime-in-quote
        user/rime-in-kbd))

(global-set-key (kbd "C-\\") 'rime-toggle)

(add-hook 'm4d-insert-mode-hook 'rime-update-input-method-state)
(add-hook 'm4d-insert-exit-hook 'rime-update-input-method-state)

(provide 'the-rime)
