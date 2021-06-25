;;; -*- lexical-binding: t -*-

(setq-default
 ;; no start messages
 inhibit-startup-message t
 ;; don't read x resource file
 inhibit-x-resources t
 ;; no welcome screen
 inhibit-splash-screen t
 inhibit-startup-screen t
 ;; no client startup messages
 server-client-instructions nil
 ;; no startup messages
 inhibit-startup-echo-area-message t
 frame-inhibit-implied-resize t
 initial-scratch-message ""
 hl-line-sticky-flag t
 ;; prefer horizental split
 split-height-threshold nil
 split-width-threshold 120
 ;; don't create lockfiles
 create-lockfiles nil
 ;; UTF-8
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 ;; add final newline
 require-final-newline t
 ;; backup setups
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 ;; xref no prompt
 xref-prompt-for-identifier nil
 ;; mouse yank at point instead of click position.
 mouse-yank-at-point t
 ;; this fix the cursor movement lag
 auto-window-vscroll nil
 ;; window divider setup
 window-divider-default-right-width 1
 window-divider-default-bottom-width 1
 ;; don't wait for keystrokes display
 echo-keystrokes 0.01
 show-paren-style 'parenthese
 ;; overline no margin
 overline-margin 0
 ;; underline no margin
 underline-minimum-offset 0
 ;; default tab width to 4(instead of 8)
 ;; some major modes will override this
 tab-width 4
 ;; don't show cursor in non selected window.
 cursor-in-non-selected-windows nil
 comment-empty-lines t
 visible-cursor nil
 ;; improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 ;; allow resize by pixels
 frame-resize-pixelwise t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 ;; indent with whitespace by default
 indent-tabs-mode nil
 ;; larger process output buffer
 read-process-output-max (* 1024 1024)
 ;; Don't truncate lines in a window narrower than 65 chars.
 truncate-partial-width-windows 65
 ;; Default line number width.
 display-line-numbers-width 3
 ;; Don't display comp warnings
 warning-suppress-log-types '((comp))
 ;; Firefox as default browser
 browse-url-browser-function 'browse-url-firefox
 ;; Custom file path
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; prefer y or n
 y-or-n-p-use-read-key t
 ;; always follow link
 vc-follow-symlinks t
 ;; disable visual line move
 line-move-visual t
 ;; case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; use short answer
 read-answer-short t
 ;; move cursor to top/bottom before signaling a scroll error
 scroll-error-top-bottom t
 ;; pinentry
 epa-pinentry-mode 'loopback
 ;; disable input method in pgtk
 pgtk-use-im-context-on-new-connection nil)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'conf-mode-hook 'visual-line-mode)
;; (add-hook 'prog-mode-hook 'hl-line-mode)
;; (add-hook 'conf-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; kmacro keys
(global-set-key (kbd "C-,") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.") 'kmacro-end-or-call-macro)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))

(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)
(setq-default max-mini-window-height 0.1)

(provide 'init-defaults)
