;; Relative line number

(bind-key "C-S-L" 'display-line-numbers-mode)

;; (defun user/update-line-number-relative ()
;;   (when display-line-numbers
;;     (setq-local display-line-numbers
;;                 (if (or m4d-normal-mode buffer-read-only)
;;                     'relative
;;                   t))))

;; (add-hook 'm4d-insert-modal-hook 'user/update-line-number-relative)
;; (add-hook 'm4d-normal-modal-hook 'user/update-line-number-relative)
;; (add-hook 'display-line-numbers-mode-hook #'user/update-line-number-relative)

;; Highlight current line.
(when (display-graphic-p)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'conf-mode-hook 'hl-line-mode)
  (add-hook 'text-mode-hook 'hl-line-mode))

(use-package hideshow
  :bind
  (("C-S-H" . 'hs-toggle-hiding))
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(use-package highlight-numbers
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package paren-face
  :init
  (setq paren-face-regexp "[()]")
  (global-paren-face-mode 1))

(use-package focus
  :bind
  (("C-S-F" . 'focus-mode)))

;; Vertical Border
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(provide 'the-visual)
