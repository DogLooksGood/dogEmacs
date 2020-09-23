;;; -*- lexical-binding: t -*-

(defun +meow-setup ()
  (meow-normal-define-key
   '("S" . kmacro-start-macro)
   '("E" . kmacro-end-macro)
   '("A" . apply-macro-to-region-lines)
   '("M" . kmacro-call-macro))

  (meow-leader-define-key
   '(";" . dired-sidebar-toggle-sidebar)
   '("k" . kill-buffer)
   '("l" . goto-line)
   '("h" . ace-window)
   '("w" . ace-swap-window)
   '("z" . +eval-and-bound-to-c-z)
   '("i" . counsel-imenu)
   '("n" . dumb-jump-go)
   '("j" . sp-join-sexp)
   '("(" . sp-wrap-round)
   '("[" . sp-wrap-square)
   '("{" . sp-wrap-curly)
   '("o" . delete-other-windows)
   '("-" . split-window-below)
   '("/" . swiper)
   '("\\" . split-window-right)
   '("'" . paredit-meta-doublequote)
   '("*" . ivy-pass)
   '("#" . deft)
   '("m" . magit-status)
   '("M" . magit-blame)
   '("p" . projectile-find-file)
   '("b" . counsel-switch-buffer)
   '("g" . projectile-ripgrep)
   '("f" . find-file)
   '("F" . find-file-literally)
   '("y" . tiny-expand)
   '("a" . emamux:send-region)
   '("!" . +open-work-log)
   ;; '("$" . +send-clojure-ns-form)
   '("." . highlight-symbol-at-point)
   '("," . unhighlight-regexp)
   '("|" . +toggle-theme)))

(use-package meow
  :quelpa
  ;; (meow :repo "DogLooksGood/meow" :fetcher github)
  (meow :fetcher file :path "~/Projects/meow")
  :config
  (+meow-setup)
  (meow-global-mode 1)
  (add-to-list 'meow-normal-state-mode-list 'restclient-mode)
  (add-to-list 'meow-normal-state-mode-list 'slack-message-buffer-mode)
  (add-to-list 'meow-normal-state-mode-list 'slack-thread-message-buffer-mode)
  (add-to-list 'meow-normal-state-mode-list 'messages-buffer-mode)
  (add-to-list 'meow-normal-state-mode-list 'alchemist-iex-mode)
  (add-to-list 'meow-normal-state-mode-list 'inf-iex-mode)
  :custom
  (meow-layout 'dvp))



;;; New concept:

;;; Use universal argument to move to the end of that thing.
;;; Use numeric argument to mark multiple at once.
;;; How `word' command work?
;;; W - to selection current word.
;;;   with universal arg, to the end of sentence
;;;   with numeric arg, mark multiple word at once.
;;;   if there's mark already, expand.
;;; M - mark current word and reverse the selection.
;;;   with universal arg, to the begin of sentence
;;;   with numeric arg, mark multiple word at once.
;;; E - mark current symbol or exp
;;;   with universal arg, to the end of block
;;;   with numeric arg, mark multiple symbols or exps
;;; Fixes

(defun meow--with-universal-argument-p (arg)
  (equal '(4) arg))

;;; char movements

(defun meow-head (arg)
  "Move towards to the head of this line.

Will cancel all other selection, except char selection.

Use with universal argument to move to beginning of line.
Use with numeric argument to move multiple chars at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-beginning-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-beginning-position)))
      (backward-char count)
      (when (< (point) bound)
        (goto-char bound))))))

(defun meow-tail (arg)
  "Move towards to the end of this line.

Will cancel all other selection, except char selection.

Use with universal argument to move to beginning of line.
Use with numeric argument to move multiple chars at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-end-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-end-position)))
      (forward-char count)
      (when (> (point) bound)
        (goto-char bound))))))

;;; char selections

(defun meow-head-select (arg)
  "Activate char selection, then move towards to the head of this line.

See `meow-head' for how prefix arguments work."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-beginning-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-beginning-position)))
      (backward-char count)
      (when (< (point) bound)
        (goto-char bound))))))

(defun meow-tail-select (arg)
  "Activate char selection, then move towards to the end of this line.

See `meow-tail' for how prefix arguments work."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (line-end-position)))
   (t
    (let ((count (prefix-numeric-value arg))
          (bound (line-end-position)))
      (forward-char count)
      (when (> (point) bound)
        (goto-char bound))))))

;;; line movements

(defun meow-prev-line (arg)
  "Move to the previous line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the first line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'previous-line))))))

(defun meow-next-line (arg)
  "Move to the next line.

Will cancel all other selection, except char selection.

Use with universal argument to move to the last line of buffer.
Use with numeric argument to move multiple lines at once."
  (interactive "P")
  (unless (eq (meow--selection-type) 'char)
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'next-line))))))

;;; line selections

(defun meow-prev-line-select (arg)
  "Activate char selection, then move to previous line.

See `meow-prev-line' for how prefix arguments work."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'previous-line))))))

(defun meow-next-line-select (arg)
  "Activate char selection, then move to previous line.

See `meow-prev-line' for how prefix arguments work."
  (interactive "P")
  (unless (region-active-p)
    (-> (meow--make-selection 'char (point) (point))
        (meow--select)))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-max)))
   (t
    (let ((count (prefix-numeric-value arg)))
      (dotimes (i count)
        (call-interactively #'next-line))))))

;;; other char selection

(defun meow-begin-of-buffer ()
  "Mark from current point, to the beginning of buffer with char selection."
  (interactive)
  (-> (meow--make-selection 'transient (point) (point-min))
      (meow--select)))

(defun meow-end-of-buffer ()
  "Mark from current point, to the end of buffer with char selection."
  (interactive)
  (-> (meow--make-selection 'transient (point) (point-max))
      (meow--select)))

(provide 'init-meow)
