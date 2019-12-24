;;; packages for NAV

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
