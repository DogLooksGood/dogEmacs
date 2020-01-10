;;; -*- lexical-binding: t -*-

(defvar m4d--last-select nil
  "Last select behavior, used by `m4d-forward-select' and `m4d-backward-select'.")

(defvar m4d-insert-modal-hook nil
  "A hook runs when we enter the insert modal.")

(defvar m4d-normal-modal-hook nil
  "A hook runs when we enter the normal modal.")

(defvar m4d-enable-mode-list
  '(json-mode
    eshell-mode)
  "A list of major-modes where we should enable modal edit.")

(defvar m4d-kill-line-kbd-macro "C-k")
(defvar m4d-delete-char-kbd-macro "C-d")
(defvar m4d-find-ref-kbd-macro "M-.")
(defvar m4d-slurp-kbd-macro "C-)")
(defvar m4d-barf-kbd-macro "C-(")
(defvar m4d-keyboard-quit-kbd-macro "C-g")
(defvar m4d-search-kbd-macro "C-s")
(defvar m4d-yank-pop-kbd-macro "ESC-y")
(defvar m4d-execute-extended-command-kbd-macro "M-x")
(defvar m4d-indent-kbd-macro "C-M-\\")

(defun m4d--mc-prompt-once-advice (fn &rest args)
  (setq mc--this-command (lambda () (interactive) (apply fn args)))
  (apply fn args))

(defun m4d--mc-prompt-once (&rest fns)
  (dolist (fn fns)
    (advice-add fn :around #'m4d--mc-prompt-once-advice)))

(make-variable-buffer-local 'm4d--last-select)

(defmacro m4d--silent (&rest body)
  "Execute body without print any message."
  `(cl-letf (((symbol-function 'message)
              (lambda (&rest args) nil)))
     ,@body))

(defun m4d--should-enable ()
  (or (equal major-mode 'fundamental-mode)
      view-mode
      (member major-mode m4d-enable-mode-list)
      (derived-mode-p 'text-mode 'conf-mode 'prog-mode)))

(defun m4d--update-cursor-shape ()
  (if (and (m4d--should-enable) (not m4d-normal-mode))
      (setq cursor-type '(bar . 5))
    (setq cursor-type 'box)))

(defun m4d--direction-right-p ()
  (or (not (mark))
      (< (mark) (point))))

(defun m4d--clear-select ()
  (setq m4d--last-select nil)
  (deactivate-mark))

(defun m4d--execute-kbd-macro (keys)
  (when-let ((cmd (key-binding (read-kbd-macro keys))))
    (call-interactively cmd)))

(defun m4d--select-thing (thing)
  (setq m4d--last-select thing)
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (push-mark beg t t)
      (goto-char end)
      t)))

;;; navigation command

(defun m4d-head ()
  (interactive)
  (m4d--clear-select)
  (call-interactively #'left-char))

(defun m4d-head-select ()
  (interactive)
  (unless (region-active-p)
    (push-mark (point) t t))
  (call-interactively #'left-char))

(defun m4d-tail ()
  (interactive)
  (m4d--clear-select)
  (call-interactively #'right-char))

(defun m4d-tail-select ()
  (interactive)
  (unless (region-active-p)
    (push-mark (point) t t))
  (call-interactively #'right-char))

(defun m4d-prev ()
  (interactive)
  (m4d--clear-select)
  (call-interactively #'previous-line))

(defun m4d-prev-select ()
  (interactive)
  (unless (region-active-p)
    (push-mark (point) t t))
  (call-interactively #'previous-line))

(defun m4d-next ()
  (interactive)
  (m4d--clear-select)
  (call-interactively #'next-line))

(defun m4d-next-select ()
  (interactive)
  (unless (region-active-p)
    (push-mark (point) t t))
  (call-interactively #'next-line))

;;; selection

;;; not support numeric argument
(defun m4d-find (arg ch)
  (interactive "P\ncFind:")
  (when-let ((pos (save-mark-and-excursion
                    (forward-char)
                    (search-forward (char-to-string ch) nil t arg))))
    (push-mark (point) t t)
    (goto-char pos)))

(defun m4d-till (arg ch)
  (interactive "P\ncTill:")
  (when-let ((pos (save-mark-and-excursion
                    (forward-char)
                    (search-forward (char-to-string ch) nil t arg))))
    (push-mark (point) t t)
    (goto-char pos)
    (backward-char)))

(defun m4d-line (arg)
  (interactive "P")
  (if (and (region-active-p) (equal 'line m4d--last-select))
      (if (m4d--direction-right-p)
          (progn
            (forward-line arg)
            (goto-char (line-end-position)))
        (progn
          (previous-line arg)
          (goto-char (line-beginning-position))))
    (progn
      (push-mark (line-beginning-position) t t)
      (goto-char (line-end-position))
      (setq m4d--last-select 'line))))

(defun m4d-exp (arg)
  (interactive "P")
  (when (equal 'line m4d--last-select)
    (m4d--clear-select))
  (if (region-active-p)
      (ignore-errors
        (if (m4d--direction-right-p)
            (forward-sexp arg)
          (backward-sexp arg)))
    (progn
      ;; this allow us to select the expression when cursor at some place like:
      ;; )|)  | is the cursor.
      (when (and (char-before)
                 (string-match-p "\\s)" (char-to-string (char-before))))
        (backward-list))
      (when (let ((pos (point)))
              (save-mark-and-excursion
                (back-to-indentation)
                (< pos (point))))
        (back-to-indentation))
      (m4d--select-thing 'sexp))))

(defun m4d-mark ()
  (interactive)
  (when (equal 'line m4d--last-select)
    (m4d--clear-select))
  (cond
   ((equal 'word m4d--last-select)
    (or (m4d--select-thing 'sexp)
        (m4d--select-thing 'list)
        (m4d--select-thing 'line)))

   ((equal 'sexp m4d--last-select)
    (or (m4d--select-thing 'list)
        (m4d--select-thing 'line)))

   ((equal 'list m4d--last-select)
    (m4d--select-thing 'list))

   (t
    (or (m4d--select-thing 'word)
        (m4d--select-thing 'sexp)
        (m4d--select-thing 'list)))))

(defun m4d-word (arg)
  (interactive "P")
  (m4d--clear-select)
  (push-mark (point) t t)
  (forward-word (prefix-numeric-value arg))
  (setq m4d--last-select 'word))

(defun m4d-backward-word (arg)
  (interactive "P")
  (m4d--clear-select)
  (push-mark (point) t t)
  (backward-word (prefix-numeric-value arg))
  (setq m4d--last-select 'word))

(defun m4d-flip ()
  (interactive)
  (when (region-active-p)
    (call-interactively #'exchange-point-and-mark)))

(defun m4d-execute-command ()
  (interactive)
  (m4d--execute-kbd-macro m4d-execute-extended-command-kbd-macro))

;;; modification

(defun m4d-open-line ()
  (interactive)
  (m4d--clear-select)
  (goto-char (line-end-position))
  (newline-and-indent)
  (m4d-insert))

(defun m4d-open-line-up ()
  (interactive)
  (m4d--clear-select)
  (goto-char (line-beginning-position))
  (newline)
  (previous-line)
  (indent-for-tab-command)
  (m4d-insert))

(defun m4d-insert-after ()
  (interactive)
  (when (and (mark) (region-active-p))
    (goto-char (max (mark) (point)))
    (m4d--clear-select))
  (m4d-normal-mode -1)
  (m4d--update-cursor-shape))

(defun m4d-insert ()
  (interactive)
  (when (and (mark) (region-active-p))
    (goto-char (min (mark) (point)))
    (m4d--clear-select))
  (m4d-normal-mode -1)
  (m4d--update-cursor-shape)
  (run-hooks 'm4d-insert-modal-hook))

(defun m4d-slurp ()
  "Forward slurp the paren, call the command of keybinding \"C-)\"."
  (interactive)
  (m4d--execute-kbd-macro m4d-slurp-kbd-macro))

(defun m4d-barf ()
  "Forward barf the paren, call the command of keybinding \"C-(\"."
  (interactive)
  (m4d--execute-kbd-macro m4d-barf-kbd-macro))

(defun m4d-kill (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (m4d--execute-kbd-macro m4d-delete-char-kbd-macro)))

(defun m4d-join ()
  (interactive)
  (call-interactively #'join-line))

(defun m4d-replace ()
  (interactive)
  (if (region-active-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (m4d-insert))
    (message "No selection!")))

(defun m4d-delete ()
  (interactive)
  (m4d--execute-kbd-macro m4d-delete-char-kbd-macro))

(defun m4d-copy ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (message "No selection!")))

(defun m4d-yank (arg)
  (interactive "P")
  (m4d--clear-select)
  (if arg
      (m4d--execute-kbd-macro m4d-yank-pop-kbd-macro)
    (yank)))

(defun m4d-query-replace ()
  (m4d--clear-select)
  (call-interactively #'query-replace))

(defun m4d-skip-cursor ()
  (interactive)
  (call-interactively #'mc/skip-to-next-like-this))

(defun m4d-indent ()
  (interactive)
  (m4d--execute-kbd-macro m4d-indent-kbd-macro))

;;; other command

(defun m4d-to-register ()
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively #'copy-to-register)
    (keyboard-quit))
   ((equal last-command 'kmacro-end-macro)
    (call-interactively #'kmacro-to-register))
   (current-prefix-arg
    (call-interactively #'window-configuration-to-register))
   (t
    (call-interactively #'point-to-register))))

(defun m4d-cc ()
  (interactive)
  (when-let ((cmd (key-binding (read-kbd-macro "C-c C-c"))))
    (call-interactively cmd)))

(defun m4d-pop-ref ()
  (interactive)
  (xref-pop-marker-stack))

(defun m4d-find-ref ()
  (interactive)
  (m4d--execute-kbd-macro m4d-find-ref-kbd-macro))

(defun m4d-search ()
  (interactive)
  (call-interactively #'swiper))

(defun m4d-quit ()
  (interactive)
  (setq m4d--last-select nil)
  (setq m4d--is-selecting nil)
  (m4d--execute-kbd-macro m4d-keyboard-quit-kbd-macro))

;;; global command

(defun m4d-insert-exit ()
  (interactive)
  (when (and (fboundp 'company-mode)
             (company--active-p))
    (company-abort))
  (m4d-normal-mode 1)
  (m4d--update-cursor-shape)
  (run-hooks 'm4d-insert-modal-hook))

(defun m4d-esc ()
  (interactive)
  (cond
   ((minibufferp)
    (call-interactively #'keyboard-escape-quit))
   (multiple-cursors-mode
    (m4d-insert-exit))
   (m4d-normal-mode
    (mode-line-other-buffer))
   ((m4d--should-enable)
    (m4d-insert-exit))
   (t
    (mode-line-other-buffer)))
  (m4d--update-cursor-shape))

;;; Define key helpers

(defun m4d-leader-define-key (key def)
  (define-key m4d-leader-keymap (kbd key) def))

(defun m4d-normal-define-key (key def)
  (define-key m4d-normal-keymap (kbd key) def))

;;; Keymaps

(defvar m4d-special-keymap nil)
(setq m4d-special-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "\\") 'split-window-right)
    (define-key keymap (kbd "@") 'split-window-below)
    (define-key keymap (kbd "q") 'delete-window)
    (define-key keymap (kbd ";") 'other-window)
    (define-key keymap (kbd "'") 'delete-other-windows)
    (define-key keymap (kbd "}") 'scroll-up)
    (define-key keymap (kbd "{") 'scroll-down)
    keymap))

(defvar m4d-keymap nil)
(setq m4d-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'm4d-esc)
    keymap))

(defvar m4d-leader-keymap nil)
(setq m4d-leader-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "x") 'eval-defun)
    (define-key keymap (kbd "e") 'eval-last-sexp)
    (define-key keymap (kbd "E") 'eval-buffer)
    (define-key keymap (kbd "SPC") 'm4d-cc)
    (define-key keymap (kbd "y") 'counsel-yank-pop)
    (define-key keymap (kbd "o") 'find-file)
    (define-key keymap (kbd "v") 'find-alternate-file)
    (define-key keymap (kbd "b") 'switch-to-buffer)
    (define-key keymap (kbd "w") 'save-buffer)
    (define-key keymap (kbd "r") 'counsel-register)
    (define-key keymap (kbd "i") 'back-to-indentation)
    (define-key keymap (kbd "q") 'save-buffers-kill-terminal)
    (define-key keymap (kbd "m") 'imenu)
    (define-key keymap (kbd "g") 'goto-line)
    (define-key keymap (kbd "(") 'paredit-wrap-round)
    (define-key keymap (kbd "[") 'paredit-wrap-square)
    (define-key keymap (kbd "{") 'paredit-wrap-curly)
    (define-key keymap (kbd ",") 'm4d-pop-ref)
    (define-key keymap (kbd ".") 'm4d-find-ref)
    (define-key keymap (kbd ";") 'comment-dwim)
    keymap))

(defvar m4d-normal-keymap nil)
(setq m4d-normal-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap m4d-special-keymap)
    (define-key keymap (kbd "1") 'digit-argument)
    (define-key keymap (kbd "2") 'digit-argument)
    (define-key keymap (kbd "3") 'digit-argument)
    (define-key keymap (kbd "4") 'digit-argument)
    (define-key keymap (kbd "5") 'digit-argument)
    (define-key keymap (kbd "6") 'digit-argument)
    (define-key keymap (kbd "7") 'digit-argument)
    (define-key keymap (kbd "8") 'digit-argument)
    (define-key keymap (kbd "9") 'digit-argument)
    (define-key keymap (kbd "0") 'digit-argument)
    (define-key keymap (kbd "a") 'm4d-insert-after)
    (define-key keymap (kbd "i") 'm4d-insert)
    (define-key keymap (kbd "h") 'm4d-head)
    (define-key keymap (kbd "t") 'm4d-tail)
    (define-key keymap (kbd "n") 'm4d-next)
    (define-key keymap (kbd "p") 'm4d-prev)
    (define-key keymap (kbd "M-h") 'm4d-head-select)
    (define-key keymap (kbd "M-t") 'm4d-tail-select)
    (define-key keymap (kbd "M-n") 'm4d-next-select)
    (define-key keymap (kbd "M-p") 'm4d-prev-select)
    (define-key keymap (kbd ",") 'm4d-till)
    (define-key keymap (kbd ".") 'm4d-find)
    (define-key keymap (kbd "e") 'm4d-exp)
    (define-key keymap (kbd "l") 'm4d-line)
    (define-key keymap (kbd "w") 'm4d-word)
    (define-key keymap (kbd "v") 'mc/mark-next-like-this)
    (define-key keymap (kbd "V") 'mc/skip-to-next-like-this)
    (define-key keymap (kbd "s") 'mc/mark-all-in-region-regexp)
    (define-key keymap (kbd "b") 'm4d-backward-word)
    (define-key keymap (kbd "f") 'm4d-flip)
    (define-key keymap (kbd "o") 'm4d-open-line)
    (define-key keymap (kbd "O") 'm4d-open-line-up)
    (define-key keymap (kbd "m") 'm4d-mark)
    (define-key keymap (kbd "r") 'm4d-replace)
    (define-key keymap (kbd "k") 'm4d-kill)
    (define-key keymap (kbd "j") 'm4d-join)
    (define-key keymap (kbd ")") 'm4d-slurp)
    (define-key keymap (kbd "(") 'm4d-barf)
    (define-key keymap (kbd "d") 'm4d-delete)
    (define-key keymap (kbd "c") 'm4d-copy)
    (define-key keymap (kbd "y") 'm4d-yank)
    (define-key keymap (kbd "/") 'm4d-search)
    (define-key keymap (kbd "=") 'm4d-indent)
    (define-key keymap (kbd "x") 'm4d-execute-command)
    (define-key keymap (kbd "[") 'beginning-of-buffer)
    (define-key keymap (kbd "]") 'end-of-buffer)
    (define-key keymap (kbd "z") 'universal-argument)
    (define-key keymap (kbd "-") 'negative-argument)
    (define-key keymap (kbd "u") 'undo)
    (define-key keymap (kbd "g") 'm4d-quit)
    (define-key keymap (kbd "%") 'm4d-query-replace)
    (define-key keymap (kbd "+") 'm4d-to-register)
    (define-key keymap (kbd "*") 'register-to-point)
    (define-key keymap (kbd "SPC") m4d-leader-keymap)
    keymap))

(defun m4d--mc-setup ()
  ;; this make it to prompt only once
  (m4d--mc-prompt-once #'m4d-till #'m4d-find)
  (add-to-list 'mc/cmds-to-run-once 'm4d-esc)
  (add-to-list 'mc/cmds-to-run-once 'm4d-quit))

;;;###autoload
(define-minor-mode m4d-normal-mode
  "m4d normal modal state."
  nil
  " <N>"
  m4d-normal-keymap)

;;;###autoload
(define-minor-mode m4d-special-mode
  "m4d special mode"
  nil
  nil
  m4d-special-keymap)

;;;###autoload
(define-minor-mode m4d-mode
  "Modal for Dvorak."
  nil
  " m4d"
  m4d-keymap
  (progn
    (m4d--mc-setup)))

;;;###autoload
(define-global-minor-mode m4d-global-mode m4d-mode
  (lambda ()
    (m4d-mode 1)
    (when (m4d--should-enable)
      (m4d-normal-mode 1))
    (when (derived-mode-p 'special-mode)
      (m4d-special-mode 1))))

(provide 'm4d)
