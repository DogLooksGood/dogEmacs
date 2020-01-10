;; Relative line number

(bind-key "C-S-L" 'display-line-numbers-mode)

(defun user/update-line-number-relative ()
  (when display-line-numbers
    (setq-local display-line-numbers
                (if (or god-local-mode buffer-read-only)
                    'relative
                  t))))

(when (fboundp 'god-local-mode)
  (add-hook 'god-local-mode-hook #'user/update-line-number-relative)
  (add-hook 'display-line-numbers-mode-hook #'user/update-line-number-relative))

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

;;; Bug of yascroll
;;; In emacs 27, yascroll doesn't work.
;; (require 'cl)
;; (use-package yascroll
;;   :init
;;   (setq yascroll:delay-to-hide 5)
;;   (global-yascroll-bar-mode))

(use-package focus
  :bind
  (("C-S-F" . 'focus-mode)))

(provide 'the-visual)
