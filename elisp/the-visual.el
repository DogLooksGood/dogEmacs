;;; Relative line number

(bind-key "C-S-L" 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun user/update-line-number-relative ()
  (when display-line-numbers
    (setq-local display-line-numbers
                (if (or god-local-mode buffer-read-only)
                    'relative
                  t))))

(add-hook 'god-local-mode-hook #'user/update-line-number-relative)
(add-hook 'display-line-numbers-mode-hook #'user/update-line-number-relative)

(use-package highlight-symbol
  :bind
  (("M-n" . 'highlight-symbol-next)
   ("M-p" . 'highlight-symbol-prev))
  :init
  (setq highlight-symbol-idle-delay 0.2)
  (setq highlight-symbol-highlight-single-occurrence nil)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package leerzeichen
  :bind
  (("C-S-W" . 'leerzeichen-mode))
  :config
  (setq leerzeichen-line-feed-glyph (make-glyph-code ?¬ 'leerzeichen))
  (add-hook 'prog-mode-hook 'leerzeichen-mode))

(use-package hideshow
  :bind
  (("C-S-H" . 'hs-toggle-hiding))
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;;; Bug of yascroll
(use-package cl)
(use-package yascroll
  :init
  (require 'cl)
  (setq yascroll:delay-to-hide 3)
  (global-yascroll-bar-mode))

(use-package focus
  :bind
  (("C-S-F" . 'focus-mode)))

(provide 'the-visual)
