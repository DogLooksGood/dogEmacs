;;; -*- lexical-binding: t -*-

;;; Faces

(defface m4d-visual-indicator
  '((((class color) (background dark))
      (:inherit font-lock-string-face))
     (((class color) (background light))
      (:inherit font-lock-string-face)))
  "Visual indicator"
  :group 'm4d)

(defface m4d-insert-indicator
  '((((class color) (background dark))
      (:inherit font-lock-function-name-face))
     (((class color) (background light))
      (:inherit font-lock-function-name-face)))
  "Insert indicator"
  :group 'm4d)

(defface m4d-motion-indicator
  '((((class color) (background dark))
      (:inherit font-lock-string-face))
     (((class color) (background light))
      (:inherit font-lock-string-face)))
  "Motion indicator"
  :group 'm4d)

;;; Custom Variables

(defvar m4d-insert-modal-hook nil
  "A hook runs when we enter the insert modal.")

(defvar m4d-normal-modal-hook nil
  "A hook runs when we enter the normal modal.")

(defvar m4d-motion-escape-mode-list nil
  "A list of major modes which allow escape to visual mode by press ESC.")
(setq m4d-motion-escape-mode-list
      '(eshell-mode
        cider-repl-mode))

(defvar m4d-motion-mode-list nil
  "A list of modes should be treated as special mode .")
(setq m4d-motion-mode-list
      '(dired-mode
        ripgrep-search-mode
        help-mode
        compilation-mode
        eshell-mode))

(defvar m4d-enable-mode-list
  '(json-mode)
  "A list of major-modes where we should enable modal edit.")

;;; Internal command translation.

(defvar m4d--eldoc-commands nil)
(setq m4d--eldoc-commands
      '(m4d-head
        m4d-tail
        m4d-prev
        m4d-next
        m4d-exp
        m4d-word
        m4d-backward-word))

(defvar m4d-kill-line-kbd-macro "C-k"
  "The kbd macro used in `m4d-kill'.")

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

(defvar m4d-newline-kbd-macro "RET"
  "The kbd macro used in `m4d-newline'.")

(defvar m4d-other-window-kbd-macro "C-x o"
  "The kbd macro used in `m4d-other-window'.")

(defvar m4d-eval-last-sexp-kbd-macro "C-x C-e"
  "The kbd macro used in `m4d-eval-last-sexp'.")

(defvar m4d-eval-defun-kbd-macro "C-M-x"
  "The kbd macro used in `m4d-eval-defun'.")

(defvar m4d-digit-1-kbd-macro "C-1")
(defvar m4d-digit-2-kbd-macro "C-2")
(defvar m4d-digit-3-kbd-macro "C-3")
(defvar m4d-digit-4-kbd-macro "C-4")
(defvar m4d-digit-5-kbd-macro "C-5")
(defvar m4d-digit-6-kbd-macro "C-6")
(defvar m4d-digit-7-kbd-macro "C-7")
(defvar m4d-digit-8-kbd-macro "C-8")
(defvar m4d-digit-9-kbd-macro "C-9")
(defvar m4d-digit-0-kbd-macro "C-0")

;;; Internal Variables

(defvar m4d--last-select nil
  "The last select behavior.")

(defvar m4d--specific-vars nil)

(defvar m4d--selections nil
  "All selections")

(defvar m4d--leader-mode-keymaps nil
  "Leader keymaps used for major modes.")

(setq m4d--specific-vars
      '(m4d--last-select))

(eval-after-load 'multiple-cursors
  (dolist (it m4d--specific-vars)
    (add-to-list 'mc/cursor-specific-vars it)))

(defun m4d-no-op ()
  (interactive))

(defun m4d--mc-prompt-once-advice (fn &rest args)
  (setq mc--this-command (lambda () (interactive) (apply fn args)))
  (apply fn args))

(defun m4d--mc-prompt-once (&rest fns)
  (dolist (fn fns)
    (advice-add fn :around #'m4d--mc-prompt-once-advice)))

(defun m4d--should-enable-motion-p ()
  (or (member major-mode m4d-motion-mode-list)
      (derived-mode-p 'special-mode)))

(defun m4d--should-enable ()
  (and (not (member major-mode m4d-motion-escape-mode-list))
       (or (equal major-mode 'fundamental-mode)
           view-mode
           (member major-mode m4d-enable-mode-list)
           (derived-mode-p 'text-mode 'conf-mode 'prog-mode))))

(defun m4d--update-cursor-shape ()
  (if (and (m4d--should-enable) (not m4d-normal-mode))
      (setq cursor-type '(bar . 3))
    (setq cursor-type 'box)))

(defun m4d--direction-right-p ()
  (if (region-active-p)
      (>= (point) (mark))
    t))

(defun m4d--start-select ()
  (push-mark (point) t t))

(defun m4d--clear-select ()
  (when m4d--last-select
    (setq m4d--last-select nil))
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

(defun m4d--in-string-p ()
  "Return if we are in string."
  (nth 3 (syntax-ppss)))

;;; digit argument

(defun m4d-digit-0 ()
  (m4d--execute-kbd-macro m4d-digit-0-kbd-macro))
(defun m4d-digit-1 ()
  (m4d--execute-kbd-macro m4d-digit-1-kbd-macro))
(defun m4d-digit-2 ()
  (m4d--execute-kbd-macro m4d-digit-2-kbd-macro))
(defun m4d-digit-3 ()
  (m4d--execute-kbd-macro m4d-digit-3-kbd-macro))
(defun m4d-digit-4 ()
  (m4d--execute-kbd-macro m4d-digit-4-kbd-macro))
(defun m4d-digit-5 ()
  (m4d--execute-kbd-macro m4d-digit-5-kbd-macro))
(defun m4d-digit-6 ()
  (m4d--execute-kbd-macro m4d-digit-6-kbd-macro))
(defun m4d-digit-7 ()
  (m4d--execute-kbd-macro m4d-digit-7-kbd-macro))
(defun m4d-digit-8 ()
  (m4d--execute-kbd-macro m4d-digit-8-kbd-macro))
(defun m4d-digit-9 ()
  (m4d--execute-kbd-macro m4d-digit-9-kbd-macro))

;;; navigation command

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

(defun m4d-kill ()
  "Semantic kill, will call the kbdmacro C-k.
 It is supposed to bind C-k with commands like `paredit-kill' or `sp-kill-hybrid-sexp'."
  (interactive)
  (if (not (region-active-p))
      (m4d--execute-kbd-macro m4d-kill-line-kbd-macro)
    (when (and (equal 'line m4d--last-select)
               (m4d--direction-right-p)
               (< (point) (point-max)))
      (forward-char 1))
    (m4d--execute-kbd-macro m4d-kill-region-kbd-macro)))

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
  (previous-line (prefix-numeric-value arg))
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
  (next-line (prefix-numeric-value arg))
  (setq m4d--last-select 'char))

(defun m4d-next-select (arg)
  (interactive "P")
  (m4d--keep-select)
  (next-line (prefix-numeric-value arg))
  (setq m4d--last-select 'char))

;;; selection

(defun m4d-end-of-line ()
  (interactive)
  (m4d--clear-select)
  (push-mark (line-end-position) t t))

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
    (while (not (or (= (point) (point-max))
                    (looking-at "\n\\|\\s)")))
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
    (unless (m4d--select-thing 'sexp t)
      (looking-at "\\s)")
      (backward-sexp)
      (m4d--select-thing 'sexp t)
      (m4d-exchange)))

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

(defun m4d--line-select-p ()
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (and (eq beg (save-mark-and-excursion (goto-char beg) (line-beginning-position)))
           (eq end (save-mark-and-excursion (goto-char end) (line-end-position)))))))

(defun m4d-forward-line (arg)
  (interactive "P")
  (unless (equal 'line m4d--last-select)
    (m4d--clear-select))
  (if (equal 'line m4d--last-select)
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
  (setq m4d--last-select 'line))

(defun m4d-kmacro ()
  (interactive)
  (when (stringp (kmacro-start-macro nil))
    (kmacro-end-macro nil)))

(defun m4d-kmacro-call (arg)
  (interactive "P")
  (kmacro-call-macro arg t))

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
  (setq m4d--last-select 'list))

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

;;; pagination

(defun m4d-select ()
  (interactive)
  (unless multiple-cursors-mode
    (if (region-active-p)
        (call-interactively #'mc/mark-all-in-region-regexp)
      (call-interactively #'isearch-forward-regexp)))
  (setq m4d--last-select nil))

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

(defun m4d-mark-whole-buffer ()
  (interactive)
  (if (equal m4d--last-select 'buffer)
      (m4d-exchange)
    (mark-whole-buffer)
    (setq m4d--last-select 'buffer)))

;;; modification

(defun m4d-open-line ()
  (interactive)
  (if (m4d--should-enable-motion-p)
      (goto-char (point-max))
    (unless (region-active-p)
      (goto-char (line-end-position)))
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
  (m4d-normal-mode -1))

(defun m4d-insert ()
  (interactive)
  (when (and (mark) (region-active-p))
    (goto-char (min (mark) (point)))
    (m4d--clear-select))
  (m4d-normal-mode -1)
  (run-hooks 'm4d-insert-modal-hook))

(defun m4d-slurp ()
  "Forward slurp the paren, call the command of keybinding \"C-)\"."
  (interactive)
  (m4d--execute-kbd-macro m4d-slurp-kbd-macro))

(defun m4d-barf ()
  "Forward barf the paren, call the command of keybinding \"C-(\"."
  (interactive)
  (m4d--execute-kbd-macro m4d-barf-kbd-macro))

(defun m4d-join ()
  (interactive)
  (call-interactively #'join-line))

(defun m4d-replace ()
  (interactive)
  (if (region-active-p)
      (progn
        (m4d--execute-kbd-macro m4d-kill-region-kbd-macro)
        (m4d-insert)
        (when (eq (line-end-position) (line-beginning-position))
          (lisp-indent-line)))
    (message "No selection!")))

(defun m4d-newline ()
  (interactive)
  (if (m4d--should-enable-motion-p)
      (progn
        (goto-char (point-max))
        (m4d-insert-after))
    (m4d--clear-select)
    (newline-and-indent)
    (m4d-insert)))

(defun m4d-backward-delete ()
  (interactive)
  (m4d--clear-select)
  (m4d--execute-kbd-macro m4d-backward-delete-char-kbd-macro))

(defun m4d-delete ()
  (interactive)
  (m4d--execute-kbd-macro m4d-delete-char-kbd-macro))

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
      (m4d--execute-kbd-macro m4d-kill-ring-save-kbd-macro)
    (message "No selection!")))

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

(defun m4d-leader (arg)
  "Don't support digit argument yet, can't figure out the reason."
  (interactive "P")
  (let ((keymap (or (m4d--get-mode-leader-keymap major-mode)
                    m4d-leader-base-keymap)))
    (set-transient-map keymap)
    (cond
     ((equal '(4) arg)
      (universal-argument)))))

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

(defun m4d-other-window ()
  (interactive)
  (m4d--execute-kbd-macro m4d-other-window-kbd-macro))

(defun m4d-quit ()
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
      (delete-window)
    (save-buffers-kill-terminal)))

(defun m4d-c-g ()
  (interactive)
  (m4d--clear-select)
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
  (run-hooks 'm4d-normal-modal-hook))

(defun m4d-esc ()
  (interactive)
  (cond
   ((m4d--should-enable-motion-p)
    (cond
     (m4d-normal-mode
      (mode-line-other-buffer))
     ((member major-mode m4d-motion-escape-mode-list)
      (m4d-insert-exit))
     (t
      (mode-line-other-buffer))))
   ((minibufferp)
    (call-interactively #'keyboard-escape-quit))
   (multiple-cursors-mode
    (m4d-insert-exit))
   (m4d-normal-mode
    (mode-line-other-buffer))
   ((m4d--should-enable)
    (m4d-insert-exit))
   (t
    (mode-line-other-buffer))))

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
(defvar m4d-motion-keymap nil)
(setq m4d-motion-keymap
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "}") 'm4d-page-down)
        (define-key keymap (kbd "{") 'm4d-page-up)
        (define-key keymap (kbd "[") 'm4d-buffer-begin)
        (define-key keymap (kbd "]") 'm4d-buffer-end)
        (define-key keymap (kbd "q") 'm4d-quit)
        keymap))

(defvar m4d-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'm4d-esc)
    (define-key keymap (kbd "C-u") 'm4d-esc)
    (define-key keymap (kbd "M-<Tab>") 'other-window)
    (define-key keymap (kbd "C-M-i") 'other-window)
    (define-key keymap (kbd "M-SPC") 'm4d-leader)
    keymap))

(defvar m4d-leader-base-keymap nil)
(setq m4d-leader-base-keymap
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "SPC") 'm4d-execute-command)
        (define-key keymap (kbd "cc") 'm4d-cc)
        keymap))

(defvar m4d-normal-keymap nil)
(setq m4d-normal-keymap
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap m4d-motion-keymap)
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
        (define-key keymap (kbd "b") 'm4d-block-expand)
        (define-key keymap (kbd "B") 'm4d-mark-whole-buffer)
        (define-key keymap (kbd "c") 'm4d-copy)
        (define-key keymap (kbd "d") 'm4d-delete)
        (define-key keymap (kbd "D") 'm4d-duplicate-line)
        (define-key keymap (kbd "e") 'm4d-exp)
        (define-key keymap (kbd "E") 'm4d-exp-select)
        (define-key keymap (kbd "f") 'm4d-flip)
        (define-key keymap (kbd "g") 'm4d-c-g)
        (define-key keymap (kbd "h") 'm4d-head)
        (define-key keymap (kbd "H") 'm4d-head-select)
        (define-key keymap (kbd "i") 'm4d-insert)
        (define-key keymap (kbd "j") 'm4d-join)
        (define-key keymap (kbd "k") 'm4d-kill)
        (define-key keymap (kbd "l") 'm4d-forward-line)
        (define-key keymap (kbd "m") 'm4d-backward-word)
        (define-key keymap (kbd "M") 'm4d-backward-word-select)
        (define-key keymap (kbd "n") 'm4d-next)
        (define-key keymap (kbd "N") 'm4d-next-select)
        (define-key keymap (kbd "o") 'm4d-open-line)
        (define-key keymap (kbd "O") 'm4d-open-line-up)
        (define-key keymap (kbd "p") 'm4d-prev)
        (define-key keymap (kbd "P") 'm4d-prev-select)
        (define-key keymap (kbd "q") 'm4d-quit)
        (define-key keymap (kbd "r") 'm4d-replace)
        (define-key keymap (kbd "R") 'm4d-replace-with-yank)
        (define-key keymap (kbd "s") 'save-buffer)
        (define-key keymap (kbd "t") 'm4d-tail)
        (define-key keymap (kbd "T") 'm4d-tail-select)
        (define-key keymap (kbd "u") 'undo)
        (define-key keymap (kbd "v") 'mc/mark-next-like-this)
        (define-key keymap (kbd "V") 'mc/skip-to-next-like-this)
        (define-key keymap (kbd "w") 'm4d-word)
        (define-key keymap (kbd "W") 'm4d-word-select)
        (define-key keymap (kbd "x") 'm4d-exchange)
        (define-key keymap (kbd "y") 'm4d-yank)
        (define-key keymap (kbd "z") 'recenter-top-bottom)
        (define-key keymap (kbd "Z") 'reposition-window)
        (define-key keymap (kbd ".") 'm4d-find-ref)
        (define-key keymap (kbd ",") 'm4d-pop-ref)
        (define-key keymap (kbd "+") 'universal-argument)
        (define-key keymap (kbd "-") 'negative-argument)
        (define-key keymap (kbd "*") 'm4d-to-register)
        (define-key keymap (kbd "&") 'register-to-point)
        (define-key keymap (kbd ")") 'm4d-slurp)
        (define-key keymap (kbd "(") 'm4d-barf)
        (define-key keymap (kbd "/") 'm4d-search)
        (define-key keymap (kbd ";") 'm4d-comment)
        (define-key keymap (kbd "$") 'm4d-end-of-line)
        (define-key keymap (kbd "?") 'mc/mark-all-in-region-regexp)
        (define-key keymap (kbd "=") 'm4d-indent)
        (define-key keymap (kbd "!") 'm4d-query-replace)
        (define-key keymap (kbd "SPC") 'm4d-leader)
        keymap))

(defun m4d--mc-setup ()
  ;; this make it to prompt only once
  (m4d--mc-prompt-once #'m4d-select)
  (add-to-list 'mc/cmds-to-run-for-all 'm4d-esc)
  (add-to-list 'mc/cmds-to-run-once 'm4d-mark)
  (add-to-list 'mc/cmds-to-run-once 'mc/mark-all-in-region)
  (add-to-list 'mc/cmds-to-run-once 'm4d-select)
  (add-to-list 'mc/cmds-to-run-once 'm4d-select-string))

(defun m4d--minibuffer-setup ()
  (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-map (kbd "C-u") 'keyboard-escape-quit))

(defun m4d--isearch-setup ()
  (define-key isearch-mode-map (kbd "}") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "{") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
  (define-key isearch-mode-map (kbd "C-u") 'isearch-abort))

(defun m4d--global-setup ()
  ;; These global key bindings are used for fundamental mode.
  (global-set-key (kbd "<escape>") 'm4d-esc)
  (global-set-key (kbd "C-u") 'm4d-esc))

(defun m4d-indicator ()
  (interactive)
  (cond
   (m4d-normal-mode
    (propertize "NORMAL" 'face 'm4d-visual-indicator))
   ((m4d--should-enable-motion-p)
    (propertize "MOTION" 'face 'm4d-motion-indicator))
   (t
    (propertize "INSERT" 'face 'm4d-insert-indicator))))

(defun m4d--eldoc-setup ()
  (apply #'eldoc-add-command m4d--eldoc-commands))

(defun m4d--select-window-advice (&rest args)
  (when (m4d--should-enable-motion-p)
    (m4d-normal-mode -1)))

(defun m4d--advice-setup ()
  (advice-add 'select-window :after 'm4d--select-window-advice))

;;;###autoload
(defun m4d-setup ()
  (setq delete-active-region nil)
  (m4d--global-setup)
  (m4d--mc-setup)
  (m4d--isearch-setup)
  (m4d--minibuffer-setup)
  (m4d--eldoc-setup)
  (m4d--advice-setup))

(defun m4d--normal-init ())

;;;###autoload
(define-minor-mode m4d-normal-mode
  "m4d normal modal state."
  nil
  ""
  m4d-normal-keymap
  (when m4d-normal-mode
    (m4d--normal-init)))

;;;###autoload
(define-minor-mode m4d-motion-mode
  "m4d special mode"
  nil
  nil
  m4d-motion-keymap)

;;;###autoload
(define-minor-mode m4d-mode
  "Modal for Dvorak."
  nil
  " M4D"
  m4d-keymap)

;;;###autoload
(define-global-minor-mode m4d-global-mode m4d-mode
  (lambda ()
    (add-hook 'post-command-hook #'m4d--update-cursor-shape t t)
    (m4d-mode 1)
    (when (m4d--should-enable)
      (m4d-normal-mode 1))
    (when (m4d--should-enable-motion-p)
      (m4d-motion-mode 1))))

(provide 'm4d)
