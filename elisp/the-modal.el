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

(defun user/yank-on-region ()
  (interactive)
  (delete-active-region)
  (call-interactively 'yank)
  (deactivate-mark t))

(use-package selected
  :ensure t
  :bind
  (:map selected-keymap
        ("<escape>" . 'keyboard-escape-quit)
        ("<backspace>" . 'delete-region)
        ("C-y" . 'user/yank-on-region))
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

(defun user/insert-mode ()
  (interactive)
  (when (region-active-p)
    (call-interactively #'delete-region))
  (when god-local-mode
    (god-local-mode -1)))

(defun user/seek-sexp ()
  (interactive)
  (unless (equal (point) (point-max))
    (forward-char)
    (while (not (or (looking-at "\\s(")
                    (equal (point) (point-max))))
      (forward-char))))

(defun user/replace-point (ch)
  (interactive "cREPLACE WITH:")
  (delete-char 1)
  (insert-char ch)
  (backward-char))

(defun user/active-region-or-kill-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (call-interactively #'set-mark-command)))

(defun user/should-use-god-mode-p ()
  (or (equal major-mode 'fundamental-mode)
      (derived-mode-p 'conf-mode 'text-mode 'prog-mode)))

(defun user/god-mode ()
  (interactive)
  (unless god-local-mode
    (god-local-mode 1)))

(defun user/escape ()
  (interactive)
  (cond
   ((not (equal 1 (mc/num-cursors)))
    (god-local-mode 1))
   ((region-active-p)
    (call-interactively #'keyboard-escape-quit))
   ((not (user/should-use-god-mode-p))
    (mode-line-other-buffer))
   (god-local-mode
    (mode-line-other-buffer))
   (t
    (god-local-mode 1))))

(defun user/maybe-god-mode (&rest args)
  "We want to enable god-mode for every text editting mode.
Use this function on `after-change-major-mode-hook'. "
  (when (user/should-use-god-mode-p)
    (god-local-mode 1)))

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define-global ",." 'user/escape))

(use-package god-mode
  :ensure t
  :quelpa (god-mode
           :fetcher github
           :repo "DogLooksGood/god-mode")
  :bind
  (("<escape>" . 'user/escape)
   ("M-g" . 'goto-line)
   ("<M-escape>" . 'kill-buffer-and-window)
   ("C-$" . 'shell-command)
   ("C-=" . 'align-regexp)
   ("C-." . 'xref-find-definitions)
   ("C-!" . 'eval-expression)
   ("C-," . 'xref-pop-marker-stack)
   :map
   minibuffer-local-map
   ("<escape>" . 'keyboard-escape-quit)
   :map
   god-local-mode-map
   ("<escape>" . 'user/escape)
   ("<tab>" . 'user/normal-tab)
   ("i" . 'user/insert-mode)
   ("u" . 'undo)
   ("j" . 'join-line)
   ("z" . 'universal-argument)
   ("w" . 'user/active-region-or-kill-region)
   ("*" . 'point-to-register)
   ("@" . 'register-to-point)
   ("s" . 'save-buffer)
   ("q" . 'user/delete-window-or-change-buffer)
   ("\\" . 'split-window-right)
   ("-" . 'split-window-below)
   ("'" . 'delete-other-windows)
   ;; navigation
   ("r" . 'up-list)
   ("f" . 'forward-sexp)
   ("b" . 'backward-sexp)
   ("h" . 'left-char)
   ("t" . 'right-char)
   ("}" . 'scroll-up-command)
   ("{" . 'scroll-down-command)
   ("[" . 'beginning-of-buffer)
   ("]" . 'end-of-buffer))
  :init
  (advice-add 'god-local-mode :around #'user/make-silent)
  (add-hook 'after-change-major-mode-hook 'user/maybe-god-mode)
  (add-hook 'god-mode-enabled-hook 'user/update-cursor-shape)
  (add-hook 'god-mode-disabled-hook 'user/update-cursor-shape)
  (setq god-mode-can-omit-literal-key t)
  (setq god-mod-alist
	'((nil . "C-")
	  ("m" . "M-")
      ("SPC" . "C-M-")))
  (setq god-literal-key "SPC"))

(unbind-key "C-x C-g")



(provide 'the-modal)
