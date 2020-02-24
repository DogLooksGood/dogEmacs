(defvar user/rime-loaded nil)

(defun user/rime-toggle ()
  (interactive)
  (unless user/rime-loaded
    (use-package liberime-config
      :quelpa (liberime-config
               :fetcher github
               :repo "DogLooksGood/liberime"
               :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))
    (require 'rime)
    (rime-register-and-set-default)
    (setq user/rime-loaded t))
  (rime-toggle))

(defun m4d-insert-mode-p ()
  m4d-insert-mode)

(setq rime-show-candidate nil)

(defun user/rime-not-in-insert-mode ()
  (not m4d-insert-mode))

(defun user/rime-in-elisp-quote ()
  (and (equal major-mode 'emacs-lisp-mode) (equal ?` (char-before))))

(defun user/rime-in-quote ()
  (and (equal ?` (char-before)) (equal ?` (char-after))))

(defun user/rime-in-kbd ()
  (and (looking-at "</kbd>")
       (looking-back "<kbd>" 1)))

(setq rime-disable-predicates
      '(user/rime-in-elisp-quote
        rime--after-alphabet-char-p
        rime--prog-in-code-p
        user/rime-not-in-insert-mode
        user/rime-in-quote
        user/rime-in-kbd))

(global-set-key (kbd "C-\\") 'user/rime-toggle)

(add-hook 'm4d-insert-mode-hook 'rime-update-binding)
(add-hook 'm4d-insert-exit-hook 'rime-update-binding)

(provide 'the-rime)
