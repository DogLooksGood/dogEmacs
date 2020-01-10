;;; -*- lexical-binding: t -*-

;;; God Mode baked modal edit.
;;; 1. Prefer the DEFAULT
;;; 2. DON’t use modifiers
;;; 3. ONLY switch modal in text edit buffer
;;; 4. Make the command SHORT
;;; 5. TWO modes, NO global leader

(defun user/update-cursor-shape ()
  (cond
   (god-local-mode
    (setq cursor-type 'box))
   (buffer-read-only
    (setq cursor-type 'box))
   (t
    (setq cursor-type '(bar . 5)))))

(defun user/yank-on-region ()
  (interactive)
  (delete-active-region)
  (call-interactively 'yank)
  (deactivate-mark t))

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

(defun user/normal-mode ()
  (interactive)
  (unless god-local-mode
    (god-local-mode 1)))

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

(defun user/jump-paren ()
  (interactive)
  (if (or (equal (char-before) 41)
          (equal (char-before) 93)
          (equal (char-before) 125))
      (backward-sexp)
    (progn
      (while (and (not (equal (char-after) 40))
                  (not (equal (char-after) 91))
                  (not (equal (char-after) 123))
                  (not (equal (point) (line-end-position))))
        (forward-char))
      (when (or (equal (char-after) 40)
                (equal (char-after) 91)
                (equal (char-after) 123))
        (forward-sexp)))))

(defun user/should-use-god-mode-p ()
  (or (member major-mode user/god-mode-enable-mode-list)
      (equal major-mode 'fundamental-mode)
      view-mode
      (equal major-mode 'json-mode)
      (derived-mode-p 'conf-mode 'text-mode 'prog-mode)))

(defun user/god-mode ()
  (interactive)
  (unless god-local-mode
    (god-local-mode 1)))

(defun user/escape ()
  (interactive)
  (cond
   ((company--active-p)
    (company-abort)
    (when (user/should-use-god-mode-p)
      (user/normal-mode)))
   ((minibufferp)
    (call-interactively #'keyboard-escape-quit))
   (multiple-cursors-mode
    (user/normal-mode))
   (iedit-mode
    (user/normal-mode))
   ((region-active-p)
    (call-interactively #'keyboard-escape-quit))
   ((or (not (user/should-use-god-mode-p))
        god-local-mode)
    (mode-line-other-buffer))
   (t
    (user/normal-mode))))

(defun user/maybe-god-mode (&rest args)
  "We want to enable god-mode for every text editting mode.
Use this function on `after-change-major-mode-hook'. "
  (when (user/should-use-god-mode-p)
    (god-local-mode 1)))

(unless (display-graphic-p)
  (use-package key-chord
    :init
    (key-chord-define-global ".," 'user/escape)
    (advice-add 'key-chord-mode :around #'user/make-silent)
    (key-chord-mode 1)))

(use-package god-mode
  :ensure t
  :quelpa (god-mode
           :fetcher github
           :repo "DogLooksGood/god-mode")
  :bind
  (("C-M-SPC" . 'set-mark-command)
   ("<escape>" . 'user/escape)
   ("M-g" . 'goto-line)
   ("C-x C-k" . 'killbuffer)
   ("M-i" . 'backward-kill-word)
   ("<M-escape>" . 'kill-buffer-and-window)
   ("C-." . 'xref-find-definitions)
   ("C-!" . 'eval-expression)
   ("C-," . 'xref-pop-marker-stack)
   ("C-a" . 'user/move-beginning-of-line-dwim)
   :map
   minibuffer-local-map
   ("<escape>" . 'keyboard-escape-quit)
   :map
   god-local-mode-map
   ("<tab>" . 'user/normal-tab)
   ("TAB" . 'user/normal-tab)
   ("<escape>" . 'user/escape)
   ("i" . 'user/insert-mode)
   ("u" . 'undo)
   ("r" . 'repeat)
   ("s" . 'save-buffer)
   ("=" . 'align-regexp)
   ("q" . 'user/delete-window-or-previous-buffer)
   ("Q" . 'kill-buffer-and-window)
   ("\\" . 'split-window-right)
   ("@" . 'split-window-below)
   ("'" . 'delete-other-windows)
   ("j" . 'join-line)
   ("v" . 'mc/mark-next-like-this)
   ("B" . 'universal-argument)
   ("D" . 'negative-argument)
   ;; navigation
   ("f" . 'forward-sexp)
   ("b" . 'backward-word)
   ("w" . 'forward-word)
   ("h" . 'left-char)
   ("t" . 'right-char)
   ("{" . 'scroll-down)
   ("}" . 'scroll-up)
   ("[" . 'beginning-of-buffer)
   ("]" . 'end-of-buffer))
  :init
  ;; don't display the mode enable/disable message.
  (advice-add 'god-local-mode :around #'user/make-silent)
  ;; automatically enable god-mode for some major modes.
  (add-hook 'after-change-major-mode-hook 'user/maybe-god-mode)
  (add-hook 'view-mode-hook 'user/maybe-god-mode)
  ;; update cursor type when god-mode enable/disable.
  (add-hook 'god-mode-enabled-hook 'user/update-cursor-shape)
  (add-hook 'god-mode-disabled-hook 'user/update-cursor-shape)
  (setq god-mode-can-omit-literal-key t)
  (setq god-mod-alist
	'((nil . "C-")
	  ("m" . "M-")
      ("SPC" . "C-M-")))
  (setq god-literal-key "SPC"))

(bind-key "{" 'scroll-down special-mode-map)
(bind-key "}" 'scroll-up special-mode-map)

(unbind-key "C-x C-g")

(use-package selected
  :ensure t
  :bind
  (:map selected-keymap
        ("<escape>" . 'keyboard-escape-quit)
        ("<backspace>" . 'delete-region)
        ("o" . 'kill-ring-save)
        (";" . 'comment-dwim)
        ("k" . 'kill-region)
        ("C-y" . 'user/yank-on-region))
  :init
  (selected-global-mode 1))

(provide 'the-modal)
