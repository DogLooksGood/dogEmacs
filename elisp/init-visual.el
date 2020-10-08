(bind-key "C-S-L" 'display-line-numbers-mode)

(setq display-line-numbers-width 3)
(defun +update-line-number-relative ()
  (when display-line-numbers
    (setq-local display-line-numbers t)))

(add-hook 'display-line-numbers-mode-hook #'+update-line-number-relative)

(global-hl-line-mode 1)

(use-package rainbow-mode
  :hook
  ((css-mode . rainbow-mode)
   (javascript-mode . rainbow-mode)
   (mhtml-mode . rainbow-mode)
   (web-mode . rainbow-mode)))

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (("C-S-H" . 'hs-toggle-hiding)
   ("<C-tab>" . 'hs-toggle-hiding)))

(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode))

(use-package yascroll
  :init
  (global-yascroll-bar-mode 1))

(use-package olivetti
  :hook ((text-mode conf-mode prog-mode) . olivetti-mode)
  :custom
  (olivetti-body-width 120))

;; (scroll-bar-mode -1)

;; Vertical Border
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(provide 'init-visual)
