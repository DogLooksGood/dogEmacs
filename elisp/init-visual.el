;; Relative line number

(bind-key "C-S-L" 'display-line-numbers-mode)

(setq display-line-numbers-width 3)
(defun +update-line-number-relative ()
  (when display-line-numbers
    (setq-local display-line-numbers 'visual)))


(add-hook 'display-line-numbers-mode-hook #'+update-line-number-relative)

;; Highlight current line.
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'conf-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (("C-S-H" . 'hs-toggle-hiding)))

(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode))

(use-package yascroll
  :init
  (global-yascroll-bar-mode))

;; Vertical Border
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(provide 'init-visual)
