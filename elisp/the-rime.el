(defvar user/rime-loaded nil)

(make-variable-buffer-local
 (defvar user/rime-use-english t
   "Should use english"))

(defun user/rime-force-english-p ()
  user/rime-use-english)

(defun rime--after-code-char-p ()
  "当前光标是否在英文的后面。"
  (when (char-before)
    (string-match-p "[a-zA-Z%\\-_]" (char-to-string (char-before)))))

(defun user/rime-toggle ()
  (interactive)
  (unless user/rime-loaded
    (use-package liberime-config
      :quelpa (liberime-config
               :fetcher github
               :repo "DogLooksGood/liberime"
               :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))
    (use-package rime
      :quelpa (rime
               :fetcher github
               :repo "DogLooksGood/emacs-rime")
      :init
      (setq rime-disable-predicates
            '(user/rime-in-elisp-quote
              rime--after-code-char-p
              rime--prog-in-code-p
              user/rime-not-in-insert-mode
              user/rime-in-quote
              user/rime-in-org-quote
              user/rime-force-english-p
              user/rime-in-kbd))
      (setq rime-show-candidate 'message))
    (setq user/rime-loaded t))
  (rime-toggle))

(defun m4d-insert-mode-p ()
  m4d-insert-mode)

(defun m4d-normal-mode-p ()
  m4d-normal-mode)

(add-hook 'm4d-normal-mode-hook (lambda () (setq user/rime-use-english nil)))

(defun user/rime-not-in-insert-mode ()
  (not m4d-insert-mode))

(defun user/rime-in-elisp-quote ()
  (and (equal major-mode 'emacs-lisp-mode) (equal ?` (char-before))))

(defun user/rime-in-quote ()
  (and (equal ?` (char-before)) (equal ?` (char-after))))

(defun user/rime-in-org-quote ()
  (and (equal major-mode 'org-mode) (equal ?~ (char-before))))

(defun user/rime-in-kbd ()
  (and (looking-at "</kbd>")
       (looking-back "<kbd>" 1)))

(defun user/rime-force-english ()
  (interactive)
  (setq user/rime-use-english t))

(global-set-key (kbd "C-\\") 'user/rime-toggle)
(global-set-key (kbd "C-SPC") 'user/rime-force-english)

(provide 'the-rime)
