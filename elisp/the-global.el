(global-set-key (kbd "C-?") help-map)

(global-set-key (kbd "<mouse-3>") 'newline)

;; Better Defaults
(setq-default
 inhibit-x-resources t
 ;; inhibit-splash-screen t
 ;; inhibit-startup-screen t
 initial-scratch-message ""
 ;; Don't highlight line when buffer is inactive
 hl-line-sticky-flag nil
 ;; Prefer horizental split
 split-height-threshold nil
 split-width-threshold 120
 ;; Don't create lockfiles
 create-lockfiles nil
 ;; UTF-8
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 ;; Add final newline
 require-final-newline t
 ;; Larger GC threshold
 gc-cons-threshold (* 100 1024 1024)
 ;; Backup setups
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 ;; Xref no prompt
 xref-prompt-for-identifier nil
 ;; Mouse yank at point instead of click position.
 mouse-yank-at-point t
 ;; This fix the cursor movement lag
 auto-window-vscroll nil
 ;; Window divider setup
 window-divider-default-right-width 1
 window-divider-default-bottom-width 1
 window-divider-default-places t
 ;; Don't wait for keystrokes display
 echo-keystrokes 0
 show-paren-style 'parenthese
 ;; Overline no margin
 overline-margin 0
 ;; Default use tab for indentation.
 indent-tabs-mode nil
 tab-width 4
 ;; Don't show cursor in non selected window.
 cursor-in-non-selected-windows nil
 comment-empty-lines t)

(prefer-coding-system 'utf-8)

;; Distraction Free. Also move these to Xresources for faster startup.
(defun user/setup-distraction-free (&optional new-frame)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(add-hook 'after-make-frame-functions #'user/setup-distraction-free)

(user/setup-distraction-free)

;; Show matched parens
(setq show-paren-delay 0.01)
(show-paren-mode 1)

;; Add internal margin
(set-frame-parameter (selected-frame) 'internal-border-width 16)
(add-to-list 'default-frame-alist '(internal-border-width . 16))

;; Always use dir-locals.
(defun safe-local-variable-p (sym val) t)

;; Auto revert when file change.
(global-auto-revert-mode 1)

;; Auto delete when insert on region.
(delete-selection-mode t)
(transient-mark-mode t)

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Custom file path
;; Actually we don't need custom file, this file can be generated
;; accidentally, so we add this file to .gitignore and never load it.
(setq custom-file "~/.emacs.d/custom.el")

;; Only show window divider when there's more than one window.
(defun user/toggle-window-divider-and-border ()
  (if (> (count-windows) 1)
      (progn
        (window-divider-mode 1))
    (progn
      (window-divider-mode -1))))

(add-hook 'window-configuration-change-hook #'user/toggle-window-divider-and-border)

;; Replace all "yes or no" with "y or n".
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove mode line.

(defun user/project-name ()
  (let ((proj-name (projectile-project-name)))
    (if (equal proj-name "-")
        ""
      proj-name)))

(setq-default frame-title-format '("["
                                   (:eval (user/project-name))
                                   (:eval
                                    (when vc-mode
                                      (replace-regexp-in-string "^ Git" " " vc-mode)))
                                   "]"
                                   " %b%* %e <%m>"))

(setq-default cursor-type 'box)
(blink-cursor-mode -1)

(defun user/delete-window-or-switch-buffer ()
  "delete window, if failed, try switch to buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (condition-case
        e
        (delete-window)
      (error
       (counsel-switch-buffer)))))

(defun user/other-buffer ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'keyboard-escape-quit)
    (mode-line-other-buffer)))

(setq initial-major-mode 'fundamental-mode)

(defun user/new-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(defvar user/god-mode-enable-mode-list nil
  "A list of mode besides prog, conf, text, fundamental, those we should enable god-mode. ")

(defun user/make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

(bind-key "C-S-U" 'browse-url-at-point)
(bind-key "C-S-P" 'proced)
(bind-key "C-x C-n" 'user/new-buffer)
(bind-key "<XF86Copy>" 'kill-ring-save)
(bind-key "<XF86Paste>" 'yank)

(provide 'the-global)
