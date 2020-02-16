;;; m4d core commands

;;; Internal command translation.

(defvar m4d-forward-line-kbd-macro "C-n"
  "The kbd macro used in `m4d-next'")

(defvar m4d-previous-line-kbd-macro "C-p"
  "The kbd macro used in `m4d-prev'")

(defvar m4d-kill-line-kbd-macro "C-k"
  "The kbd macro used in `m4d-kill'.")

(defvar m4d-kill-whole-line-kbd-macro "<C-S-backspace>"
  "The kbd macro used in `m4d-kill'.")

(defvar m4d-delete-char-kbd-macro "C-d"
  "The kbd macro used in `m4d-delete'.")

(defvar m4d-find-ref-kbd-macro "M-."
  "The kbd macro used in `m4d-find-ref'.")

(defvar m4d-backward-slurp-kbd-macro "C-("
  "The kbd macro used in `m4d-barf'.")

(defvar m4d-slurp-kbd-macro "C-)"
  "The kbd macro used in `m4d-slurp'.")

(defvar m4d-backward-barf-kbd-macro "C-{"
  "The kbd macro used in `m4d-slurp'.")

(defvar m4d-barf-kbd-macro "C-}"
  "The kbd macro used in `m4d-barf'.")

(defvar m4d-keyboard-quit-kbd-macro "C-g"
  "The kbd macro used in `m4d-quit'.")

(defvar m4d-search-kbd-macro "C-s"
  "The kbd macro used in `m4d-search'.")

(defvar m4d-reverse-search-kbd-macro "C-r"
  "The kbd macro used in `m4d-search-backward'.")

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

(defvar m4d-meta-space-kbd-macro "M-SPC"
  "The kbd macro used in `m4d-space'")

(defvar m4d-comment-kbd-macro "M-;"
  "The kbd macro used in `m4d-comment'.")

(defvar m4d-switch-buffer-kbd-macro "C-x b"
  "The kbd macro used in `m4d-switch-buffer'.")

(defvar m4d-newline-kbd-macro "RET"
  "The kbd macro used in `m4d-newline'.")

(defvar m4d-other-window-kbd-macro "C-x o"
  "The kbd macro used in `m4d-other-window'.")

(defvar m4d-eval-last-sexp-kbd-macro "C-x C-e"
  "The kbd macro used in `m4d-eval-last-sexp'.")

(defvar m4d-eval-defun-kbd-macro "C-M-x"
  "The kbd macro used in `m4d-eval-defun'.")

;;; Digit arguments

(defun m4d-digit-1 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 1))
  (universal-argument--mode))

(defun m4d-digit-2 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 2))
  (universal-argument--mode))

(defun m4d-digit-3 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 3))
  (universal-argument--mode))

(defun m4d-digit-4 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 4))
  (universal-argument--mode))

(defun m4d-digit-5 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 5))
  (universal-argument--mode))

(defun m4d-digit-6 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 6))
  (universal-argument--mode))

(defun m4d-digit-7 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 7))
  (universal-argument--mode))

(defun m4d-digit-8 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 8))
  (universal-argument--mode))

(defun m4d-digit-9 ()
  (interactive)
  (prefix-command-preserve-state)
  (setq prefix-arg (+ (* (or prefix-arg 0) 10) 9))
  (universal-argument--mode))

;;; Navigations & Selections

(defun m4d-head-1 (arg)
  (let ((i 0))
    (while (< i (prefix-numeric-value arg))
      (unless (equal (point) (line-beginning-position))
        (left-char))
      (setq i (1+ i)))))

(defun m4d-tail-1 (arg)
  (let ((i 0))
    (while (< i (prefix-numeric-value arg))
      (unless (equal (point) (line-end-position))
        (right-char))
      (setq i (1+ i)))))

(defun m4d-head (arg)
  "Move cursor towards to head of current line.
Do nothing if always at the beginning."
  (interactive "P")
  (unless (equal 'char m4d--last-select)
    (m4d--clear-select))
  (m4d-head-1 arg)
  (setq m4d--last-select 'char))

(defun m4d-head-select (arg)
  "Same to `m4d-head' but activate the selection."
  (interactive "P")
  (m4d--keep-select)
  (m4d-head-1 arg)
  (setq m4d--last-select 'char))

(defun m4d-tail (arg)
  "Move cursor towards to tail of current line.
Do nothing if always at the end."
  (interactive "P")
  (unless (equal 'char m4d--last-select)
    (m4d--clear-select))
  (m4d-tail-1 arg)
  (setq m4d--last-select 'char))

(defun m4d-tail-select (arg)
  "Same to `m4d-tail' but activate the selection."
  (interactive "P")
  (m4d--keep-select)
  (m4d-tail-1 arg)
  (setq m4d--last-select 'char))

(defun m4d-prev (arg)
  (interactive "P")
  (unless (equal 'char m4d--last-select)
    (m4d--clear-select))
  (m4d--execute-kbd-macro m4d-previous-line-kbd-macro)
  (setq m4d--last-select 'char))

(defun m4d-prev-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (previous-line (prefix-numeric-value arg))
  (setq m4d--last-select 'char))

(defun m4d-next (arg)
  (interactive "P")
  (unless (equal 'char m4d--last-select)
    (m4d--clear-select))
  (m4d--execute-kbd-macro m4d-forward-line-kbd-macro)
  (setq m4d--last-select 'char))

(defun m4d-next-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (next-line (prefix-numeric-value arg))
  (setq m4d--last-select 'char))

(defun m4d-end-of-line ()
  (interactive)
  (m4d--clear-select)
  (push-mark (line-end-position) t t)
  (exchange-point-and-mark))

(defun m4d-begin-of-line ()
  (interactive)
  (m4d--clear-select)
  (push-mark (line-beginning-position) t t)
  (exchange-point-and-mark))

(defun m4d-back-to-indentation ()
  (interactive)
  (m4d--clear-select)
  (push-mark (save-mark-and-excursion (back-to-indentation) (point)) t t)
  (exchange-point-and-mark))

(defun m4d--flip-right ()
  (push-mark (point) t t)
  (goto-char (save-mark-and-excursion
               (ignore-errors
                 (while (< (point) (point-max)) (forward-sexp)))
               (point)))
  (if (and (fboundp 'paredit-mode) paredit-mode)
      (while (not (or (= (point) (point-max))
                      (looking-at "\\s)")))
        (forward-char))
    (while (and
            (< (point) (point-max))
            (not (or (= (point) (point-max))
                         (looking-at "\n\\|\\s)"))))
      (forward-char))))

(defun m4d--flip-left ()
  (push-mark (point) t t)
  (goto-char (save-mark-and-excursion
               (ignore-errors
                 (while (> (point) (point-min)) (backward-sexp)))
               (point)))
  (while (not (or (= (point) (point-min))
                  (looking-back "\\s(" 1)))
    (backward-char)))

(defun m4d--flip-string-right ()
  (push-mark (point) t t)
  (while (and (< (point) (point-max))
              (save-mark-and-excursion (forward-char) (m4d--in-string-p)))
    (forward-char)))

(defun m4d--flip-string-left ()
  (push-mark (point) t t)
  (while (and (> (point) (point-min))
              (save-mark-and-excursion (backward-char) (m4d--in-string-p)))
    (backward-char)))

(defun m4d-flip ()
  (interactive)
  (unless (equal 'flip m4d--last-select)
    (m4d--clear-select))
  (cond
   ((m4d--in-string-p)
    (cond
     ((not (region-active-p))
      (m4d--flip-string-right))

     ((not (m4d--direction-right-p))
      (exchange-point-and-mark)
      (m4d--clear-select)
      (m4d--flip-string-right))

     (t
      (exchange-point-and-mark)
      (m4d--clear-select)
      (m4d--flip-string-left))))

   ((not (region-active-p))
    (m4d--flip-right))

   ((not (m4d--direction-right-p))
    (exchange-point-and-mark)
    (m4d--clear-select)
    (m4d--flip-right))

   (t
    (exchange-point-and-mark)
    (m4d--clear-select)
    (m4d--flip-left)))
  (setq m4d--last-select 'flip))

(defun m4d-exp-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (ignore-errors
    (if (m4d--direction-right-p)
        (forward-sexp arg)
      (backward-sexp arg))))

(defun m4d-exp (arg)
  (interactive "P")
  (unless (or (equal 'exp m4d--last-select)
              (equal 'list m4d--last-select))
    (m4d--clear-select))
  (cond
   ((let ((pos (point)))
      (save-mark-and-excursion
        (back-to-indentation)
        (< pos (point))))
    (back-to-indentation)
    (push-mark (point) t t)
    (forward-sexp))

   ((not (region-active-p))
    (while (and (< (point) (point-max))
                (looking-at "\\s-"))
      (forward-char))
    (unless (m4d--select-thing 'sexp t)
      (when (looking-at "\\s)")
        (backward-sexp))
      (if (m4d--select-thing 'sexp t)
          (m4d-exchange)
        (ignore-errors
            (push-mark (point) t t)
            (forward-sexp)))))

   ((not (m4d--direction-right-p))
    (push-mark (point) t t)

    (condition-case err
        (backward-sexp)
      (error (forward-sexp))))

   (t
    (push-mark (point) t t)
    (condition-case err
        (forward-sexp)
      (error (backward-sexp)))))
  (when-let ((num (prefix-numeric-value arg)))
    (when (> num 0)
      (if (m4d--direction-right-p)
          (forward-sexp (1- num))
        (backward-sexp (1- num)))))
  (setq m4d--last-select 'exp))

(defun m4d-line (arg)
  (interactive "P")
  (unless (equal 'line m4d--last-select)
    (m4d--clear-select))
  (if (and (region-active-p) (equal 'line m4d--last-select))
      (if (m4d--direction-right-p)
          (progn
            (forward-line arg)
            (goto-char (line-end-position)))
        (progn
          (previous-line arg)
          (goto-char (line-beginning-position))))
    (let ((n (prefix-numeric-value arg)))
      (if (> n 0)
          (progn
            (push-mark (line-beginning-position) t t)
            (forward-line (1- n))
            (goto-char (line-end-position)))
        (push-mark (line-end-position) t t)
        (forward-line (1+ n))
        (goto-char (line-beginning-position)))))
  (setq m4d--last-select 'line)
  (m4d--save-position-record))

(defun m4d-block-expand ()
  (interactive)
  (unless (equal m4d--last-select 'list)
    (m4d--clear-select))
  (if (m4d--in-string-p)
      (progn (while (and (m4d--in-string-p)
                         (not (equal (point) (point-max))))
               (forward-char))
             (push-mark (scan-sexps (point) -1) t t))
    (m4d--select-thing 'list (m4d--direction-right-p)))
  (setq m4d--last-select 'list)
  (m4d--save-position-record))

(defun m4d-word (arg)
  (interactive "P")
  (unless (equal m4d--last-select 'word)
    (when (let ((bound (bounds-of-thing-at-point 'word)))
            (and bound
                 (<= (car bound) (point))
                 (< (point) (cdr bound))))
      (forward-word)))
  (forward-word (prefix-numeric-value arg))
  (m4d--select-thing 'word t)
  (setq m4d--last-select 'word))

(defun m4d-word-select (arg)
  (interactive "P")
  (unless (m4d--direction-right-p)
    (m4d-exchange))
  (m4d--keep-select)
  (forward-word (prefix-numeric-value arg))
  (setq m4d--last-select 'word))

(defun m4d-backward-word (arg)
  (interactive "P")
  (if (region-active-p)
      (backward-word (prefix-numeric-value arg))
    (unless (equal (point) (car (bounds-of-thing-at-point 'word)))
      (backward-word)))
  (m4d--select-thing 'word nil)
  (setq m4d--last-select 'word))

(defun m4d-backward-word-select (arg)
  (interactive "P")
  (when (m4d--direction-right-p)
    (m4d-exchange))
  (m4d--keep-select)
  (backward-word (prefix-numeric-value arg))
  (setq m4d--last-select 'word))

(defun m4d-exchange ()
  (interactive)
  (when (region-active-p)
    (exchange-point-and-mark)))

(defun m4d-god-exchange ()
  (interactive)
  (if (region-active-p)
      (m4d-exchange)
    (god-local-mode 1)
    (call-interactively #'god-mode-self-insert)))

(defun m4d-mark-whole-buffer ()
  (interactive)
  (if (equal m4d--last-select 'buffer)
      (m4d-exchange)
    (mark-whole-buffer)
    (setq m4d--last-select 'buffer)))

;;; Deletions

(defun m4d-kill ()
  "Semantic kill, will call the kbdmacro C-k.
 It is supposed to bind C-k with commands like `paredit-kill' or `sp-kill-hybrid-sexp'."
  (interactive)
  (if (not (region-active-p))
      (if (equal last-command 'm4d-c-g)
          (m4d--execute-kbd-macro m4d-kill-line-kbd-macro)
        (message "No selection!"))
    (when (and (equal 'line m4d--last-select)
               (m4d--direction-right-p)
               (< (point) (point-max)))
      (forward-char 1))
    (m4d--execute-kbd-macro m4d-kill-region-kbd-macro)))

(defun m4d-replace ()
  (interactive)
  (if (region-active-p)
      (progn
        (m4d--execute-kbd-macro m4d-kill-region-kbd-macro)
        (m4d-insert)
        (when (eq (line-end-position) (line-beginning-position))
          (indent-for-tab-command)))
    (if (not (equal last-command 'm4d-c-g))
        (message "No selection!")
      (m4d--execute-kbd-macro m4d-kill-line-kbd-macro)
      (m4d-insert))))

(defun m4d-backward-delete ()
  (interactive)
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-backward-delete-char-kbd-macro))

(defun m4d-delete ()
  (interactive)
  (m4d--execute-kbd-macro m4d-delete-char-kbd-macro))

;;; Other Commands

(defun m4d-open-line ()
  (interactive)
  (if (m4d--should-enable-motion-p)
      (goto-char (point-max))
    (goto-char (line-end-position))
    (m4d--clear-select)
    (newline-and-indent))
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
  (m4d--switch-modal 'insert))

(defun m4d-insert ()
  (interactive)
  (when (and (mark) (region-active-p))
    (goto-char (min (mark) (point)))
    (m4d--clear-select))
  (m4d--switch-modal 'insert)
  (run-hooks 'm4d-insert-modal-hook))

(defun m4d-slurp ()
  "Forward slurp the paren, call the command of keybinding \"C-)\"."
  (interactive)
  (m4d--execute-kbd-macro m4d-slurp-kbd-macro))

(defun m4d-barf ()
  "Forward barf the paren, call the command of keybinding \"C-(\"."
  (interactive)
  (m4d--execute-kbd-macro m4d-barf-kbd-macro))

(defun m4d-duplicate-line ()
  (interactive)
  (save-mark-and-excursion
    (kill-ring-save (line-beginning-position) (line-end-position))
    (goto-char (line-end-position))
    (newline)
    (yank)))

(defun m4d-copy ()
  (interactive)
  (if (region-active-p)
      (progn
        (when (and (equal 'line m4d--last-select)
                   (m4d--direction-right-p)
                   (< (point) (point-max)))
          (forward-char 1))
        (m4d--execute-kbd-macro m4d-kill-ring-save-kbd-macro))
    (message "No selection!")))

(defun m4d-god-copy ()
  (interactive)
  (if (region-active-p)
      (m4d-copy)
    (god-local-mode 1)
    (call-interactively #'god-mode-self-insert)))

(defun m4d-yank (arg)
  (interactive "P")
  (m4d--clear-select)
  (if arg
      (m4d--execute-kbd-macro m4d-yank-pop-kbd-macro)
    (yank)))

(defun m4d-replace-with-yank (beg end)
  (interactive "r")
  (when (region-active-p)
    (delete-region beg end))
  (yank))

(defun m4d-query-replace ()
  (interactive)
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-query-replace-kbd-macro))

(defun m4d-indent ()
  (interactive)
  (m4d--execute-kbd-macro m4d-indent-kbd-macro))

(defun m4d-comment ()
  (interactive)
  (m4d--execute-kbd-macro m4d-comment-kbd-macro))

(defun m4d-eval-last-sexp ()
  (interactive)
  (m4d--execute-kbd-macro m4d-eval-last-sexp-kbd-macro))

(defun m4d-eval-defun ()
  (interactive)
  (m4d--execute-kbd-macro m4d-eval-defun-kbd-macro))

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

(defun m4d-ck ()
  (interactive)
  (when-let ((cmd (key-binding (read-kbd-macro "C-c C-k"))))
    (call-interactively cmd)))

(defun m4d-join ()
  (interactive)
  (call-interactively #'join-line))

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
  (beginning-of-buffer))

(defun m4d-buffer-end ()
  (interactive)
  (end-of-buffer))

;;; Copy from multiple-cursor, change the prompt word "Mark" to "Select" the key S.
(defun m4d-select (beg end &optional search)
  (interactive "r")
  (if (region-active-p)
      (let ((search (or search (read-from-minibuffer "Select: ")))
            (case-fold-search nil))
        (if (string= search "")
            (message "Select aborted")
          (progn
            (mc/remove-fake-cursors)
            (goto-char beg)
            (while (search-forward search end t)
              (push-mark (match-beginning 0))
              (mc/create-fake-cursor-at-point))
            (let ((first (mc/furthest-cursor-before-point)))
              (if (not first)
                  (error "Search failed for %S" search)
                (mc/pop-state-from-overlay first)))
            (if (> (mc/num-cursors) 1)
                (multiple-cursors-mode 1)
              (multiple-cursors-mode 0)))))
    (message "No selection!")))

(defun m4d-pop-ref ()
  (interactive)
  (if multiple-cursors-mode
      (message "Can't pop ref when multiple cursor is enabled.")
    (m4d--clear-select)
    (xref-pop-marker-stack)))

(defun m4d-find-ref ()
  (interactive)
  (if multiple-cursors-mode
      (message "Can't find ref when multiple cursor is enabled.")
    (m4d--clear-select)
    (m4d--execute-kbd-macro m4d-find-ref-kbd-macro)))

(defun m4d-search ()
  (interactive)
  (if multiple-cursors-mode
      (message "Can't search when multiple cursor is enabled.")
    (m4d--clear-select)
    (m4d--execute-kbd-macro m4d-search-kbd-macro)))

(defun m4d-reverse-search ()
  (interactive)
  (if multiple-cursors-mode
      (message "Can't search when multiple cursor is enabled.")
    (m4d--clear-select)
    (m4d--execute-kbd-macro m4d-reverse-search-kbd-macro)))

(defun m4d-mark (arg)
  (interactive "P")
  (m4d--clear-select)
  (if (equal arg '(4))
      (call-interactively 'mc/mark-all-in-region-regexp)
    (call-interactively 'mc/mark-next-like-this)))

(defun m4d-other-window ()
  (interactive)
  (m4d--execute-kbd-macro m4d-other-window-kbd-macro))

(defun m4d-quoted-insert ()
  (interactive)
  (call-interactively 'quoted-insert))

(defun m4d-quit ()
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
    (delete-window)
    (previous-buffer)))

(defun m4d-undo ()
  (interactive)
  (if (region-active-p)
      (m4d--pop-position-record)
    (call-interactively #'undo)))

(defun m4d-c-g ()
  (interactive)
  (cond
   (multiple-cursors-mode
    (m4d--clear-select))
   (t
    (m4d--execute-kbd-macro m4d-keyboard-quit-kbd-macro))))

(defun m4d-visit-next ()
  (interactive)
  (call-interactively #'mc/mark-next-like-this))

(defun m4d-visit-skip ()
  (interactive)
  (call-interactively #'mc/skip-to-next-like-this))

(defun m4d-god ()
  (interactive)
  (god-local-mode 1)
  (call-interactively #'god-mode-self-insert))

(defun m4d-space ()
  (interactive)
  (if m4d--space-command
      (call-interactively m4d--space-command)
    (m4d--execute-kbd-macro m4d-execute-extended-command-kbd-macro)))

(defun m4d-escape-or-normal-modal ()
  (interactive)
  (cond
   (god-local-mode
    (god-local-mode -1))
   ((or multiple-cursors-mode m4d-insert-mode)
    (m4d--switch-modal 'normal))))

(defun m4d-global-esc ()
  (interactive)
  (cond
   ((minibufferp)
    (call-interactively #'minibuffer-keyboard-quit))
   ((equal major-mode 'fundamental-mode)
    (m4d--to-normal))
   (t
    (message "Add %s to either `m4d-normal-mode-list' or `m4d-motion-mode-list'."))))

(defun m4d-last-buffer ()
  (interactive)
  (mode-line-other-buffer))

(provide 'm4d-core)
