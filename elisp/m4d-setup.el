;;; Setup m4d to play well with other modes.

;;; Constants

(defconst m4d--mc-cmd-run-once
  '(mc/vertical-align-with-space
    m4d-select
    m4d-visit-next
    m4d-visit-skip
    m4d-quit
    m4d-find-ref
    m4d-pop-ref
    m4d-leader))

(defconst m4d--mc-cmd-run-for-all
  '(m4d-undo
    m4d-space
    m4d-search
    m4d-reverse-search
    m4d-insert-after
    m4d-esc
    m4d-mark-whole-buffer
    m4d-copy
    m4d-delete
    m4d-duplicate-line
    m4d-exp
    m4d-exp-select
    m4d-flip
    m4d-c-g
    m4d-head
    m4d-head-select
    m4d-insert
    m4d-join
    m4d-kill
    m4d-line
    m4d-backward-word
    m4d-backward-word-select
    m4d-next
    m4d-next-select
    m4d-open-line
    m4d-open-line-up
    m4d-prev
    m4d-prev-select
    m4d-replace
    m4d-replace-with-yank
    m4d-select
    m4d-tail
    m4d-tail-select
    m4d-word
    m4d-word-select
    m4d-exchange
    m4d-yank
    m4d-slurp
    m4d-barf
    m4d-comment
    m4d-end-of-line
    m4d-begin-of-line
    m4d-back-to-indentation
    m4d-indent))

(defconst m4d--eldoc-commands
  '(m4d-head
    m4d-tail
    m4d-prev
    m4d-next
    m4d-exp
    m4d-word
    m4d-backward-word))

(defconst m4d--specific-vars
  '(m4d--last-select))

;;; ElDoc

(defun m4d--eldoc-setup ()
  "Setup commands those trigger eldoc.
Basically, all navigation commands should trigger eldoc."
  (apply #'eldoc-add-command m4d--eldoc-commands))

;;; Multiple Cursors

(defun m4d--mc-prompt-once-advice (fn &rest args)
  (setq mc--this-command (lambda () (interactive) (apply fn args)))
  (apply fn args))

(defun m4d--mc-prompt-once (&rest fns)
  (dolist (fn fns)
    (advice-add fn :around #'m4d--mc-prompt-once-advice)))

(defun m4d--mc-setup ()
  (m4d--mc-prompt-once #'m4d-select)
  (dolist (cmd m4d--mc-cmd-run-once)
    (add-to-list 'mc/cmds-to-run-once cmd))
  (dolist (cmd m4d--mc-cmd-run-for-all)
    (add-to-list 'mc/cmds-to-run-for-all cmd))
  (dolist (it m4d--specific-vars)
    (add-to-list 'mc/cursor-specific-vars it))
  (setq mc--list-file-loaded t))

;;; Minibuffer

(defun m4d--minibuffer-setup ()
  (define-key minibuffer-local-map (kbd "<escape>") 'm4d-esc)
  (define-key minibuffer-local-map (kbd "C-u") 'keyboard-escape-quit))

;;; ISearch

(defun m4d--isearch-setup ()
  (define-key isearch-mode-map (kbd "}") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "{") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
  (define-key isearch-mode-map (kbd "C-u") 'isearch-abort))

(defun m4d--select-window-advice (&rest args)
  "Auto toggle mode when switch buffers.

The default mode is set with priority: MOTION > NORMAL > INSERT."
  (when (and (not (equal (last-buffer) (current-buffer)))
             (not (minibufferp))
             (not (minibufferp (last-buffer))))
    (when (m4d--should-enable-motion-p)
      (m4d-normal-mode -1))
    (when (m4d--should-enable-normal-p)
      (m4d-insert-exit))))

(defun m4d--toggle-modes-setup ()
  (advice-add 'select-window :after 'm4d--select-window-advice))

;;; Global keybindings

(defun m4d--global-setup ()
  ;; These global key bindings are used for fundamental mode.
  (global-set-key (kbd "<escape>") 'm4d-esc)
  (global-set-key (kbd "C-u") 'm4d-esc))

(defun m4d-setup ()
  ;; This is important, otherwise we have to deactivate region before delete char.
  (setq delete-active-region nil)
  (m4d--global-setup)
  (m4d--mc-setup)
  (m4d--isearch-setup)
  (m4d--minibuffer-setup)
  (m4d--eldoc-setup)
  (m4d--toggle-modes-setup)
  (m4d--kmacro-mode-setup))

(provide 'm4d-setup)
