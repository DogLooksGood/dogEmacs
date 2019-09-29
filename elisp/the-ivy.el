(use-package ivy
  :ensure t
  :bind
  (("C-'" . 'ivy-resume)
   :map ivy-minibuffer-map
   ("<escape>" . keyboard-escape-quit)
   ("{" . ivy-previous-line)
   ("}" . ivy-next-line)
   ("<tab>" . ivy-alt-done))
  :init
  (setq ivy-use-virtual-buffers nil
	ivy-use-selectable-prompt t)
  (ivy-mode 1))

;; (use-package ivy-posframe
;;   :init
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-mode 1))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(provide 'the-ivy)
