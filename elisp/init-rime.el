;;; -*- lexical-binding: t -*-

(defun +rime-predicate-in-code-string ()
  (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-string-face))

(use-package rime
  ;; :quelpa
  ;; (rime :fetcher file :path "~/Projects/emacs-rime" :files ("rime.el" "rime-predicates.el" "lib.c" "Makefile"))
  :bind
  (:map
   rime-active-mode-map
   ("<tab>" . 'rime-inline-ascii)
   :map rime-mode-map
   ("C-`" . 'rime-send-keybinding)
   ("M-j" . 'rime-force-enable)
   ("C-SPC" . 'toggle-input-method))
  :custom
  ((rime-disable-predicates '(meow-normal-mode-p
                              meow-motion-mode-p
                              meow-keypad-mode-p
                              +rime-predicate-in-code-string
                              rime-predicate-prog-in-code-p
                              rime-predicate-after-alphabet-char-p))
   (rime-inline-predicates '(rime-predicate-space-after-cc-p
                             rime-predicate-current-uppercase-letter-p))
   (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
   (rime-inline-ascii-holder ?a)
   (default-input-method "rime")
   (rime-cursor "˰")
   (rime-show-candidate 'posframe)
   (rime-posframe-fixed-position t)
   (rime-posframe-properties (list
                              :internal-border-width 2
                              :internal-border-color "#303030"))))

;;; Used for package developing

;; (setq rime-librime-root "~/.emacs.d/librime/dist")
;; (add-to-list 'load-path "~/develop/emacs-rime/")
;; (require 'rime)
;; (setq default-input-method "rime")
;; (setq rime-show-candidate 'posframe)
;; (setq rime-cursor "˰")
;; (bind-key "M-j" 'rime-force-enable rime-mode-map)
;; (bind-key "M-j" 'rime-inline-ascii rime-active-mode-map)
;; (bind-key "C-SPC" 'toggle-input-method)
;; (bind-key "C-$" 'rime-send-keybinding rime-mode-map)
;; (setq rime-disable-predicates '(+rime-predicate-not-in-insert-mode
;;                                 rime-predicate-prog-in-code-p
;;                                 rime-predicate-after-alphabet-char-p))
;; (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
;;                                rime-predicate-current-uppercase-letter-p))
;; (add-hook 'change-major-mode-after-body-hook 'deactivate-input-method)
;; (setq rime-posframe-properties
;;  (list :background-color "#333333"
;;        :foreground-color "#dcdccc"
;;        :font "unifont"
;;        :internal-border-width 10))

(provide 'init-rime)
