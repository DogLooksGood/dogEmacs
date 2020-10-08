;;; -*- lexical-binding: t -*-

(defun display-startup-echo-area-message ())

;; Better Defaults
(setq-default
 inhibit-startup-message t
 inhibit-x-resources t
 inhibit-splash-screen t
 inhibit-startup-screen t
 frame-inhibit-implied-resize t
 initial-major-mode 'fundamental-mode
 initial-scratch-message ""
 ;; Don't highlight line when buffer is inactive
 hl-line-sticky-flag t
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
 tab-width 4
 ;; Don't show cursor in non selected window.
 cursor-in-non-selected-windows nil
 comment-empty-lines t
 visible-cursor t
 ;; Improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 ;; Allow resize by pixels
 frame-resize-pixelwise t
 x-gtk-resize-child-frames nil)

;;; Fix underline display
(setq underline-minimum-offset 0)
(setq x-underline-at-descent-line t)

(setq-default indent-tabs-mode nil)

(prefer-coding-system 'utf-8)

;; Show matched parens
(setq show-paren-delay 0.01)
(show-paren-mode -1)

;; Always use dir-locals.
(defun safe-local-variable-p (sym val) t)

;; Auto revert when file change.
(global-auto-revert-mode 1)

;; Delete trailing whitespace on save.
(defun +setup-delete-trailing-whitespace ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t))

(add-hook 'prog-mode-hook '+setup-delete-trailing-whitespace)

(global-subword-mode 1)

;; Custom file path
;; Actually we don't need custom file, this file can be generated
;; accidentally, so we add this file to .gitignore and never load it.
(setq custom-file "~/.emacs.d/custom.el")

;; Replace all "yes or no" with "y or n".
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default cursor-type 'box)
(blink-cursor-mode -1)

(defun +delete-window-or-previous-buffer ()
  "delete window, if failed, try switch to buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (condition-case
        e
        (delete-window)
      (error
       (previous-buffer)))))

(defun +other-buffer ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'keyboard-escape-quit)
    (mode-line-other-buffer)))

(defun +move-beginning-of-line-dwim (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun +new-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(defun +make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

;;; Isearch Setup
(define-key isearch-mode-map (kbd "}") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "{") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)

(defun +safe-suspend-frame ()
  (interactive)
  (unless (display-graphic-p)
    (suspend-frame)))

;; (define-key global-map (kbd "C-x C-z") '+safe-suspend-frame)
;; (define-key global-map (kbd "C-z") '+safe-suspend-frame)

(defun +eval-and-bound-to-c-z ()
  (interactive)
  (bind-key "C-z" (eval-defun nil)))

(global-set-key (kbd "<M-next>") 'scroll-other-window)
(global-set-key (kbd "<M-prior>") 'scroll-other-window-down)
(global-set-key (kbd "C-M-S-d") 'toggle-debug-on-error)

(defvar +use-icons nil
  "Use icons?")

(provide 'init-global)
