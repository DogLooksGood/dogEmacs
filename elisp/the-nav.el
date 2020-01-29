;;; packages for NAV

(use-package highlight-symbol
  :bind
  (("M-n" . 'highlight-symbol-next)
   ("M-p" . 'highlight-symbol-prev))
  :init
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-highlight-single-occurrence nil)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package swiper
  :commands (swiper)
  :ensure t
  :bind
  (("C-/" . swiper)
   :map
   special-mode-map
   ("/" . swiper)))

(provide 'the-nav)
