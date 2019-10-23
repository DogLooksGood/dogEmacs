;;; Relative line number

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
  :custom-face
  (form-feed-line ((t (:strike-through "#666666"))))
  :init
  (add-hook 'prog-mode-hook #'form-feed-mode)
  :config
  (set-face-attribute 'form-feed-line nil :strike-through "#666"))

(use-package highlight-symbol
  :bind
  (("C-S-H" . 'highlight-symbol)
   ("M-n" . 'highlight-symbol-next)
   ("M-p" . 'highlight-symbol-prev))
  :init
  (setq highlight-symbol-idle-delay nil)
  (highlight-symbol-mode 1))

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
