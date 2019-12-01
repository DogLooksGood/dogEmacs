(use-package ivy
  :ensure t
  :bind
  (("M-z" . 'ivy-resume)
   :map ivy-minibuffer-map
   ("<escape>" . 'keyboard-escape-quit)
   ("{" . 'ivy-previous-line)
   ("}" . 'ivy-next-line)
   ("<mouse-3>". 'ivy-done)
   ("<tab>" . 'ivy-alt-done))
  :init
  (setq ivy-use-virtual-buffers nil
	ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package counsel
  :bind
  ("C-<escape>" . 'counsel-ibuffer)
  ("C-? C-f" . 'counsel-describe-function)
  ("C-x C-h" . 'counsel-imenu)
  ("C-x C-b" . 'counsel-ibuffer))

(use-package swiper
  :commands (swiper)
  :ensure t
  :bind
  ("C-s" . swiper))

(provide 'the-ivy)
