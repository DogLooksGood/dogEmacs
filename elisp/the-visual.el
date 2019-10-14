;;; Relative line number

;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

(bind-key "C-S-L" 'display-line-numbers-mode)

(defun user/update-line-number-relative ()
  (when display-line-numbers
    (setq-local display-line-numbers
                (if (or god-local-mode buffer-read-only)
                    'relative
                  t))))

(add-hook 'god-local-mode-hook #'user/update-line-number-relative)
(add-hook 'display-line-numbers-mode-hook #'user/update-line-number-relative)

(use-package form-feed
  :ensure t
  :custom-face
  (form-feed-line ((t (:strike-through "#666"))))
  :init
  (add-hook 'prog-mode-hook #'form-feed-mode)
  :config
  (set-face-attribute 'form-feed-line nil :strike-through "#666"))

(use-package highlight-symbol
  :custom-face
  (highlight-symbol-face ((t (:underline "#666"))))
  :bind
  (("M-n" . 'highlight-symbol-next)
   ("M-p" . 'highlight-symbol-prev))
  :init
  (highlight-symbol-mode 1))

(use-package hideshow
  :bind
  (("C-S-H" . 'hs-toggle-hiding))
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(use-package dimmer
  :init
  (dimmer-mode))

(use-package focus
  :bind
  (("C-S-F" . 'focus-mode)))

(provide 'the-visual)
