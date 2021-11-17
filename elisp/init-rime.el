;;; -*- lexical-binding: t -*-

(straight-use-package '(rime :type git :host github :repo "DogLooksGood/emacs-rime"))

(defun +rime-predicate-org-syntax-punc-p ()
  (when (eq major-mode 'org-mode)
    (member rime--current-input-key '(91 93 42 126))))

(defun +rime-predicate-md-syntax-punc-p ()
  (when (eq major-mode 'markdown-mode)
    (member rime--current-input-key '(91 93 96))))

(setq
 rime-disable-predicates '(meow-normal-mode-p
                           meow-motion-mode-p
                           meow-keypad-mode-p
                           +rime-predicate-org-syntax-punc-p
                           +rime-predicate-md-syntax-punc-p)
 rime-inline-predicates '(rime-predicate-space-after-cc-p
                          rime-predicate-current-uppercase-letter-p
                          +rime-predicate-md-syntax-punc-p)
 rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g")
 rime-inline-ascii-holder ?a
 default-input-method "rime"
 rime-cursor "|"
 rime-show-candidate 'minibuffer
 window-min-height 1
 rime-title " ㄓ")

(autoload #'toggle-input-method "rime" nil t)

(global-set-key (kbd "C-SPC") 'toggle-input-method)
(global-set-key (kbd "C-@") 'toggle-input-method)

(with-eval-after-load "rime"
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(provide 'init-rime)
