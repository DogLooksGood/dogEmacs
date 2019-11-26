(defun user/update-cursor-shape ()
  (cond
   (god-local-mode
    (setq cursor-type 'box))

   (buffer-read-only
    (setq cursor-type 'hollow))

   (t
    (setq cursor-type '(bar . 5)))))

(defun user/ensure-insert-mode ()
  (when god-local-mode
    (god-local-mode -1)))

(use-package selected
  :ensure t
  :bind
  (:map selected-keymap
	("<backspace>" . 'delete-region))
  :init
  (selected-global-mode 1))

(defun user/make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

(defun user/insert-after ()
  (interactive)
  (forward-char)
  (god-local-mode -1))

(use-package god-mode
  :ensure t
  :bind
  (("C-x C-k" . 'kill-buffer)
   ("C-x C-x" . 'save-buffer)
   ("M-g" . 'goto-line)
   ("C-;" . 'comment-dwim)
   ("C-=" . 'indent-region)
   ("C-." . 'xref-find-definitions)
   ("C-," . 'xref-pop-marker-stack)
   :map
   minibuffer-local-map
   ("<escape>" . 'keyboard-escape-quit)
   :map
   text-mode-map
   ("<escape>" . 'god-local-mode)
   :map
   conf-mode-map
   ("<escape>" . 'god-local-mode)
   :map
   prog-mode-map
   ("<escape>" . 'god-local-mode)
   :map
   god-local-mode-map
   ("<escape>" . mode-line-other-buffer)
   ("i" . 'god-local-mode)
   ("z" . 'undo)
   ("/" . 'swiper)
   ("s" . 'save-buffer)
   ("r" . 'repeat)
   ("j" . 'join-line)
   ;; navigation
   ("f" . 'forward-sexp)
   ("b" . 'backward-sexp)
   ("h" . 'backward-char)
   ("t" . 'forward-char)
   ("P" . 'backward-page)
   ("N" . 'forward-page)
   ("}" . 'scroll-up-command)
   ("{" . 'scroll-down-command)
   ("v" . 'avy-goto-word-or-subword-1)
   ("[" . 'beginning-of-buffer)
   ("]" . 'end-of-buffer)
   ("o" . 'set-mark-command))
  :init
  (advice-add 'god-local-mode :around #'user/make-silent)
  (add-hook 'text-mode-hook 'god-local-mode)
  (add-hook 'prog-mode-hook 'god-local-mode)
  (add-hook 'conf-mode-hook 'god-local-mode)
  (add-hook 'god-mode-enabled-hook 'user/update-cursor-shape)
  (add-hook 'god-mode-disabled-hook 'user/update-cursor-shape)
  (setq god-mod-alist
	'((nil . "C-")
	  ("m" . "M-")
      ("SPC" . "C-M-")))
  (setq god-literal-key "SPC"))

(defun user/insert-mode ()
  (interactive)
  (when god-local-mode
    (god-local-mode -1)))

(provide 'the-modal)
