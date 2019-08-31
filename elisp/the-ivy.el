(use-package ivy
  :ensure t
  :bind
  (("M-f" . 'counsel-find-file)
   ("M-b" . 'counsel-ibuffer)
   :map ivy-minibuffer-map
   ("<escape>" . keyboard-escape-quit)
   ("{" . ivy-previous-line)
   ("}" . ivy-next-line)
   ("C-<escape>" . ivy-resume)
   ("<tab>" . ivy-alt-done))
  :init
  (setq ivy-use-virtual-buffers nil
	ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(provide 'the-ivy)
