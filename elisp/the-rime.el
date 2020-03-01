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

(use-package liberime-config
  :defer t
  :quelpa (liberime-config
           :fetcher github
           :repo "DogLooksGood/liberime"
           :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))

(use-package rime
  :quelpa (rime
           ;; :fetcher github
           ;; :repo "DogLooksGood/emacs-rime"
           :fetcher file
           :path "~/develop/emacs-rime/rime.el")
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

;; (add-to-list 'load-path "~/develop/emacrs-rime")
;; (require 'rime)
;; (setq rime-disable-predicates
;;       '(user/rime-in-elisp-quote
;;         rime--after-code-char-p
;;         rime--prog-in-code-p
;;         user/rime-not-in-insert-mode
;;         user/rime-in-quote
;;         user/rime-in-org-quote
;;         user/rime-force-english-p
;;         user/rime-in-kbd)
;;       rime-show-candidate 'message)

(defun m4d-insert-mode-p ()
  m4d-insert-mode)

(defun m4d-normal-mode-p ()
  m4d-normal-mode)

(add-hook 'm4d-normal-mode-hook (lambda () (setq user/rime-use-english nil)))

(defun user/rime-force-english ()
  (interactive)
  (setq user/rime-use-english (not user/rime-use-english)))

(define-key m4d-insert-keymap (kbd "C-\\") 'user/rime-force-english)

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

(global-set-key (kbd "C-SPC") 'rime-toggle)
(global-unset-key (kbd "C-\\"))


(provide 'the-rime)
