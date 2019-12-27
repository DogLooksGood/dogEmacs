;;; packages for NAV

(use-package highlight-symbol
  :bind
  (("C-}" . 'highlight-symbol-next)
   ("C-{" . 'highlight-symbol-prev))
  :init
  (setq highlight-symbol-idle-delay 0.2)
  (setq highlight-symbol-highlight-single-occurrence nil)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package avy
  :bind
  ("C-M-SPC" . 'avy-goto-word-or-subword-1)
  :init
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-setup-default))

(use-package swiper
  :commands (swiper)
  :ensure t
  :bind
  (("C-/" . swiper)
   :map
   special-mode-map
   ("/" . swiper)))

(provide 'the-nav)
