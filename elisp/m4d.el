;;; -*- lexical-binding: t -*-

;;; Custom Variables

(defvar m4d-insert-modal-hook nil
  "A hook runs when we enter the insert modal.")

(defvar m4d-normal-modal-hook nil
  "A hook runs when we enter the normal modal.")

(defvar m4d-enable-mode-list
  '(json-mode)
  "A list of major-modes where we should enable modal edit.")

;;; Internal command translation.

(defvar m4d-kill-line-kbd-macro "C-k"
  "The kbd macro used in `m4d-zap'.")

(defvar m4d-delete-char-kbd-macro "C-d"
  "The kbd macro used in `m4d-delete'.")

(defvar m4d-find-ref-kbd-macro "M-."
  "The kbd macro used in `m4d-find-ref'.")

(defvar m4d-slurp-kbd-macro "C-)"
  "The kbd macro used in `m4d-slurp'.")

(defvar m4d-barf-kbd-macro "C-}"
  "The kbd macro used in `m4d-barf'.")

(defvar m4d-keyboard-quit-kbd-macro "C-g"
  "The kbd macro used in `m4d-quit'.")

(defvar m4d-search-kbd-macro "C-s"
  "The kbd macro used in `m4d-search'.")

(defvar m4d-reverse-search-kbd-macro "C-s"
  "The kbd macro used in `m4d-search'.")

(defvar m4d-yank-pop-kbd-macro "M-y"
  "The kbd macro used in `m4d-yank'.")

(defvar m4d-execute-extended-command-kbd-macro "M-x"
  "The kbd macro used in `m4d-execute-command'.")

(defvar m4d-indent-kbd-macro "C-M-\\"
  "The kbd macro used in `m4d-indent'.")

(defvar m4d-kill-region-kbd-macro "C-w"
  "The kbd macro used in `m4d-kill'.")

(defvar m4d-kill-ring-save-kbd-macro "M-w"
  "The kbd macro used in `m4d-copy'.")

(defvar m4d-query-replace-kbd-macro "M-%"
  "The kbd macro used in `m4d-query-replace'.")

(defvar m4d-backward-delete-char-kbd-macro "DEL"
  "The kbd macro used in `m4d-backward-delete'.")

(defvar m4d-comment-kbd-macro "M-;"
  "The kbd macro used in `m4d-comment'.")

(defvar m4d-switch-buffer-kbd-macro "C-x b"
  "The kbd macro used in `m4d-switch-buffer'.")

;;; Internal Variables

(defvar m4d--last-select nil
  "The last select behavior.")

(defvar m4d--selections nil
  "All selections")

(defvar m4d--leader-mode-keymaps nil
  "Leader keymaps used for major modes.")

(defvar m4d--direction 'right
  "Current direction, can be 'left or 'right.")

(defun m4d-no-op ()
  (interactive))

;;; Selections

(defun m4d--flip-direction ()
  (setq m4d--direction
        (if (eq 'left m4d--direction)
            'right
          'left)))

(defun m4d--update-selection-display ()
  (remove-overlays (point-min) (point-max) 'name 'm4d-selection)
  (mapcar (lambda (s)
            (let* ((selection-overlay (make-overlay (car s) (cdr s))))
              (overlay-put selection-overlay 'face 'region)
              (overlay-put selection-overlay 'name 'm4d-selection)))
          m4d--selections))

(defun m4d--add-cursor-below ()
  (interactive)
  (save-mark-and-excursion
    (when (zerop (forward-line 1))
      (m4d--create-selection (point) (1+ (point))))))

(defun m4d--remove-all-selections ()
  (setq m4d--selections nil)
  (m4d--update-selection-display))

(defun m4d--create-selection (begin end)
  "Create a selection from begin to end."
  (push (cons begin end) m4d--selections)
  (m4d--update-selection-display))

(ignore
 (m4d--add-cursor-below)

 (m4d--flip-direction)

 (m4d--remove-all-selections)

 (m4d--create-selection (point) (+ 5 (point))))

(defun m4d--mc-prompt-once-advice (fn &rest args)
  (setq mc--this-command (lambda () (interactive) (apply fn args)))
  (apply fn args))

(defun m4d--mc-prompt-once (&rest fns)
  (dolist (fn fns)
    (advice-add fn :around #'m4d--mc-prompt-once-advice)))

(defun m4d--should-enable-special-mode ()
  (or (equal major-mode 'dired-mode)
      (equal major-mode 'help-mode)
      (equal major-mode 'compilation-mode)
      (derived-mode-p 'special-mode)))

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
  (or (not (region-active-p))
      (<= (mark) (point))))

(defun m4d--start-select ()
  (push-mark (point) t t))

(defun m4d--clear-select ()
  (setq m4d--last-select nil)
  (when (region-active-p)
    (deactivate-mark)))

(defun m4d--keep-select ()
  (unless (region-active-p)
    (push-mark (point) t t)))

(defun m4d--execute-kbd-macro (keys)
  (when-let ((cmd (key-binding (read-kbd-macro keys))))
    (call-interactively cmd)))

(defun m4d--select-thing (thing &optional direction-right)
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (let* ((beg (car bounds))
           (end (cdr bounds)))
      (when (and beg end)
        (m4d--clear-select)
        (push-mark (if direction-right beg end) t t)
        (goto-char (if direction-right end beg))))))

;;; navigation command

(defun m4d-head-1 (arg)
  (let ((i 0))
    (while (< i (prefix-numeric-value arg))
      (unless (equal (point) (line-beginning-position))
        (left-char))
      (setq i (1+ i)))))

(defun m4d-mark-cursors (beg end)
  (interactive "r")
  (message "%s %s" beg end))

(defun m4d-tail-1 (arg)
  (let ((i 0))
    (while (< i (prefix-numeric-value arg))
      (unless (equal (point) (line-end-position))
        (right-char))
      (setq i (1+ i)))))

(defun m4d-head (arg)
  (interactive "P")
  (m4d--clear-select)
  (m4d-head-1 arg))

(defun m4d-head-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (m4d-head-1 arg))

(defun m4d-kill ()
  (interactive)
  (m4d--execute-kbd-macro m4d-kill-line-kbd-macro))

(defun m4d-tail (arg)
  (interactive "P")
  (m4d--clear-select)
  (m4d-tail-1 arg))

(defun m4d-tail-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (m4d-tail-1 arg))

(defun m4d-prev (arg)
  (interactive "P")
  (m4d--clear-select)
  (previous-line (prefix-numeric-value arg)))

(defun m4d-prev-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (previous-line (prefix-numeric-value arg)))

(defun m4d-next (arg)
  (interactive "P")
  (m4d--clear-select)
  (next-line (prefix-numeric-value arg)))

(defun m4d-next-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (next-line (prefix-numeric-value arg)))

;;; selection

(defun m4d-exp (arg)
  (interactive "P")
  (when (and (m4d--line-select-p)
             (not (member last-command '(m4d-exp m4d-flip))))
    (m4d--clear-select))
  (if (region-active-p)
      (ignore-errors
        (if (m4d--direction-right-p)
            (forward-sexp arg)
          (backward-sexp arg)))
    (let ((should-flip))
      ;; this allow us to select the expression when cursor at position like:
      ;; )|).
      (when (looking-at "\\s)")
        (backward-sexp)
        (setq should-flip t))
      (when (let ((pos (point)))
              (save-mark-and-excursion
                (back-to-indentation)
                (< pos (point))))
        (back-to-indentation))
      (m4d--select-thing 'sexp t)
      (when (and should-flip (looking-at "\\s)"))
        (m4d-flip)))))

(defun m4d--line-select-p ()
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (and (eq beg (save-mark-and-excursion (goto-char beg) (line-beginning-position)))
           (eq end (save-mark-and-excursion (goto-char end) (line-end-position)))))))

(defun m4d-line (arg)
  (interactive "P")
  (if (m4d--line-select-p)
      (if (m4d--direction-right-p)
          (progn
            (forward-line arg)
            (goto-char (line-end-position)))
        (progn
          (previous-line arg)
          (goto-char (line-beginning-position))))
     (progn
      (push-mark (line-beginning-position) t t)
      (goto-char (line-end-position)))))

(defun m4d-exp-reverse (arg)
  (interactive "P")
  (when (region-active-p)
    (ignore-errors
      (if (m4d--direction-right-p)
          (backward-sexp arg)
        (forward-sexp arg)))))

(defun m4d-kmacro ()
  (interactive)
  (when (stringp (kmacro-start-macro nil))
    (kmacro-end-macro nil)))

(defun m4d-kmacro-call (arg)
  (interactive "P")
  (kmacro-call-macro arg t))

(defun m4d-expand ()
  (interactive)
  (m4d--select-thing 'list (m4d--direction-right-p)))

(defun m4d-word (arg)
  (interactive "P")
  (forward-word (prefix-numeric-value arg))
  (m4d--select-thing 'word t))

(defun m4d-word-select (arg)
  (interactive "P")
  (unless (m4d--direction-right-p)
    (m4d-flip))
  (m4d--keep-select)
  (forward-word (prefix-numeric-value arg)))

(defun m4d-backward-word (arg)
  (interactive "P")
  (backward-word (prefix-numeric-value arg))
  (m4d--select-thing 'word nil))

(defun m4d-backward-word-select (arg)
  (interactive "P")
  (when (m4d--direction-right-p)
    (m4d-flip))
  (m4d--keep-select)
  (backward-word (prefix-numeric-value arg)))

(defun m4d-flip ()
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark)))

;;; pagination

(defun m4d-page-up (arg)
  (interactive "P")
  (m4d--clear-select)
  (scroll-down arg))

(defun m4d-page-down (arg)
  (interactive "P")
  (m4d--clear-select)
  (scroll-up arg))

(defun m4d-buffer-begin ()
  (interactive)
  (m4d--clear-select)
  (goto-char (point-min)))

(defun m4d-buffer-end ()
  (interactive)
  (m4d--clear-select)
  (goto-char (point-max)))

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

(defun m4d-zap (arg)
  (interactive "P")
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-kill-line-kbd-macro))

(defun m4d-join ()
  (interactive)
  (call-interactively #'join-line))

(defun m4d-change ()
  (interactive)
  (if (region-active-p)
      (progn
        (m4d--execute-kbd-macro m4d-kill-region-kbd-macro)
        (m4d-insert))
    (message "No selection!")))

(defun m4d-backward-delete ()
  (interactive)
  (m4d--execute-kbd-macro m4d-backward-delete-char-kbd-macro))

(defun m4d-delete ()
  (interactive)
  (if (region-active-p)
      (m4d--execute-kbd-macro m4d-kill-region-kbd-macro)
    (m4d--execute-kbd-macro m4d-delete-char-kbd-macro)))

(defun m4d-copy ()
  (interactive)
  (if (region-active-p)
      (m4d--execute-kbd-macro m4d-kill-ring-save-kbd-macro)
    (message "No selection!")))

(defun m4d-yank (arg)
  (interactive "P")
  (m4d--clear-select)
  (if arg
      (m4d--execute-kbd-macro m4d-yank-pop-kbd-macro)
    (yank)))

(defun m4d-query-replace ()
  (interactive)
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-query-replace-kbd-macro))

(defun m4d-skip-cursor ()
  (interactive)
  (call-interactively #'mc/skip-to-next-like-this))

(defun m4d-indent ()
  (interactive)
  (m4d--execute-kbd-macro m4d-indent-kbd-macro))

(defun m4d-comment ()
  (interactive)
  (m4d--execute-kbd-macro m4d-comment-kbd-macro))

;;; other command

(defun m4d-execute-command ()
  (interactive)
  (m4d--execute-kbd-macro m4d-execute-extended-command-kbd-macro))

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

(defun m4d-leader ()
  (interactive)
  (let ((keymap (or (m4d--get-mode-leader-keymap major-mode)
                    m4d-leader-base-keymap)))
    (set-transient-map keymap)))

(defun m4d-pop-ref ()
  (interactive)
  (m4d--clear-select)
  (xref-pop-marker-stack))

(defun m4d-find-ref ()
  (interactive)
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-find-ref-kbd-macro))

(defun m4d-search ()
  (interactive)
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-search-kbd-macro))

(defun m4d-reverse-search ()
  (interactive)
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-reverse-search-kbd-macro))

(defun m4d-mark (arg)
  (interactive "P")
  (m4d--clear-select)
  (if (equal arg '(4))
      (call-interactively 'mc/mark-all-in-region-regexp)
    (call-interactively 'mc/mark-next-like-this)))

(defun m4d-quit ()
  (interactive)
  (setq m4d--last-select nil)
  (setq m4d--is-selecting nil)
  (m4d--execute-kbd-macro m4d-keyboard-quit-kbd-macro))

(defun m4d-switch-buffer ()
  (interactive)
  (m4d--execute-kbd-macro m4d-switch-buffer-kbd-macro))

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
   (phi-search--active
    (call-interactively #'phi-search-abort))
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

(defun m4d-leader-define-key (&rest args)
  (mapcar (lambda (key-def)
            (define-key m4d-leader-base-keymap
              (kbd (car key-def))
              (cdr key-def)))
          args))

(defun m4d--get-mode-leader-keymap (mode &optional ensure)
  "Return the leader keymap for mode.
If ensure is t, create new if not found."
  (if-let ((keymap (plist-get m4d--leader-mode-keymaps mode)))
      keymap
    (if ensure
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap m4d-leader-base-keymap)
        (setq m4d--leader-mode-keymaps (plist-put m4d--leader-mode-keymaps mode keymap))
        keymap)
      (when (m4d--should-enable)
        m4d-leader-base-keymap))))

(defun m4d-leader-define-mode-key (mode &rest args)
  (when-let ((keymap (m4d--get-mode-leader-keymap mode t)))
    (mapcar (lambda (key-def)
              (define-key keymap
                (kbd (car key-def))
                (cdr key-def)))
            args)))

(defun m4d-normal-define-key (&rest args)
  (mapcar (lambda (key-def)
            (define-key m4d-normal-keymap
              (kbd (car key-def))
              (cdr key-def)))
          args))

;;; Keymaps
(defvar m4d-special-keymap nil)
(setq m4d-special-keymap
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "}") 'm4d-page-down)
        (define-key keymap (kbd "{") 'm4d-page-up)
        (define-key keymap (kbd "[") 'm4d-buffer-begin)
        (define-key keymap (kbd "]") 'm4d-buffer-end)
        (define-key keymap (kbd "'") 'm4d-switch-buffer)
        (define-key keymap (kbd "\\") 'split-window-right)
        (define-key keymap (kbd "|") 'split-window-below)
        (define-key keymap (kbd "M-<tab>") 'other-window)
        (define-key keymap (kbd "M-TAB") 'other-window)
        (define-key keymap (kbd "q") 'kill-buffer-and-window)
        keymap))

(defvar m4d-keymap nil)
(setq m4d-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'm4d-esc)
    (define-key keymap (kbd "C-a") 'm4d-esc)
    keymap))

(defvar m4d-leader-base-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "SPC") 'm4d-execute-command)
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
        (define-key keymap (kbd "b") 'm4d-copy)
        (define-key keymap (kbd "c") 'm4d-change)
        (define-key keymap (kbd "d") 'm4d-delete)
        (define-key keymap (kbd "e") 'm4d-exp)
        (define-key keymap (kbd "f") 'm4d-flip)
        (define-key keymap (kbd "g") 'm4d-quit)
        (define-key keymap (kbd "h") 'm4d-head)
        (define-key keymap (kbd "H") 'm4d-head-select)
        (define-key keymap (kbd "i") 'm4d-insert)
        (define-key keymap (kbd "j") 'm4d-join)
        (define-key keymap (kbd "k") 'm4d-kill)
        (define-key keymap (kbd "l") 'recenter-top-bottom)
        (define-key keymap (kbd "L") 'reposition-window)
        (define-key keymap (kbd "m") 'm4d-backward-word)
        (define-key keymap (kbd "M") 'm4d-backward-word-select)
        (define-key keymap (kbd "n") 'm4d-next)
        (define-key keymap (kbd "N") 'm4d-next-select)
        (define-key keymap (kbd "o") 'm4d-open-line)
        (define-key keymap (kbd "o") 'm4d-open-line)
        (define-key keymap (kbd "O") 'm4d-open-line-up)
        (define-key keymap (kbd "p") 'm4d-prev)
        (define-key keymap (kbd "P") 'm4d-prev-select)
        (define-key keymap (kbd "q") 'quoted-insert)
        (define-key keymap (kbd "r") 'm4d-expand)
        (define-key keymap (kbd "s") 'mc/mark-all-in-region-regexp)
        (define-key keymap (kbd "t") 'm4d-tail)
        (define-key keymap (kbd "T") 'm4d-tail-select)
        (define-key keymap (kbd "u") 'undo)
        (define-key keymap (kbd "v") 'mc/mark-next-like-this)
        (define-key keymap (kbd "V") 'mc/skip-to-next-like-this)
        (define-key keymap (kbd "w") 'm4d-word)
        (define-key keymap (kbd "W") 'm4d-word-select)
        (define-key keymap (kbd "x") 'm4d-line)
        (define-key keymap (kbd "y") 'm4d-yank)
        (define-key keymap (kbd "z") 'repeat)
        (define-key keymap (kbd ".") 'm4d-find-ref)
        (define-key keymap (kbd ",") 'm4d-pop-ref)
        (define-key keymap (kbd "+") 'universal-argument)
        (define-key keymap (kbd "-") 'negative-argument)
        (define-key keymap (kbd "*") 'm4d-to-register)
        (define-key keymap (kbd "&") 'register-to-point)
        (define-key keymap (kbd ")") 'm4d-slurp)
        (define-key keymap (kbd "(") 'm4d-barf)
        (define-key keymap (kbd "/") 'swiper)
        (define-key keymap (kbd ";") 'm4d-comment)
        (define-key keymap (kbd "$") 'eshell)
        (define-key keymap (kbd "?") 'm4d-reverse-search)
        (define-key keymap (kbd "=") 'm4d-indent)
        (define-key keymap (kbd "!") 'm4d-query-replace)
        (define-key keymap (kbd "@") 'other-frame)
        (define-key keymap (kbd "SPC") 'm4d-leader)
        keymap))

(defun m4d--mc-setup ()
  ;; this make it to prompt only once
  (m4d--mc-prompt-once #'mc/mark-all-in-region-regexp)
  (add-to-list 'mc/cmds-to-run-for-all 'm4d-esc)
  (add-to-list 'mc/cmds-to-run-once 'm4d-mark))

(defun m4d--minibuffer-setup ()
  (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit))

(defun m4d--isearch-setup ()
  (define-key phi-search-default-map (kbd "}") 'phi-search-again-or-next)
  (define-key phi-search-default-map (kbd "{") 'phi-search-again-or-previous)
  (define-key phi-search-default-map (kbd "<escape>") 'phi-search-abort))

(defun m4d-indicator ()
  (interactive)
  (if (m4d--should-enable)
      (if m4d-normal-mode
          "VISUAL"
        "INSERT")
    ""))

;;;###autoload
(defun m4d-setup ()
  (setq delete-active-region nil)
  (m4d--mc-setup)
  (m4d--isearch-setup)
  (m4d--minibuffer-setup))

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
  m4d-keymap)

;;;###autoload
(define-global-minor-mode m4d-global-mode m4d-mode
  (lambda ()
    (unless (minibufferp)
      (m4d-mode 1))
    (when (m4d--should-enable)
      (m4d-normal-mode 1))
    (when (m4d--should-enable-special-mode)
      (m4d-special-mode 1))))

(provide 'm4d)
