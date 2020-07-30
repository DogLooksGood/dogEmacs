;;; -*- lexical-binding: t -*-
;; Look And Feels
;; mode line and themes.

(require 'storybook-theme)
(require 'joker-theme)

(setq +font-fixed-family "DejaVu Sans Mono"
      +font-variable-family "DejaVu Serif"
      +font-size 10
      +frame-margin 15
      +alpha 100
      +themes (list 'dark 'joker 'light 'storybook)
      +theme 'dark)

(defun +setup-prog-faces ()
  (face-remap-add-relative 'font-lock-function-name-face :height 140)
  (face-remap-add-relative 'font-lock-comment-face :family +font-variable-family))

(defun +setup-text-faces ()
  (face-remap-add-relative 'default :family +font-variable-family)
  (when (derived-mode-p 'org-mode)
    (face-remap-add-relative 'org-block :family +font-fixed-family)
    (face-remap-add-relative 'org-code :family +font-fixed-family)
    (face-remap-add-relative 'org-table :family +font-fixed-family)))

(defun +get-theme (dark-or-light)
  (plist-get +themes dark-or-light))

(defun +setup-theme ()
  (disable-theme (+get-theme 'dark))
  (disable-theme (+get-theme 'light))
  (load-theme (+get-theme +theme) t))

(defun +setup-font ()
  (let ((font (concat +font-fixed-family "-" (number-to-string +font-size))))
    (set-frame-font font nil t)
    (add-to-list 'default-frame-alist (cons 'font font))))

(defun +setup-transparency ()
  (set-frame-parameter nil 'alpha (cons +alpha +alpha)))

(defun +setup-internal-margin ()
  (set-frame-parameter (selected-frame) 'internal-border-width +frame-margin)
  (add-to-list 'default-frame-alist '(internal-border-width . +frame-margin)))

(defun +load-look-and-feel ()
  "Load look and feel options.

Will setup following customizations:
- transparency
- internal margin
- font
- theme
- special faces for prog-mode and text-mode(handle all existing buffer as well)."
  (interactive)
  (+setup-font)
  (+setup-transparency)
  (+setup-internal-margin)
  (+setup-theme)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (cond
       ((derived-mode-p 'prog-mode)
        (+setup-prog-faces))
       ((derived-mode-p 'text-mode)
        (+setup-text-faces))))))

(defun +toggle-theme ()
  "Toggle themes between dark and light."
  (interactive)
  (setq +theme (if (eq +theme 'dark) 'light 'dark))
  (+setup-theme))

;;; Hook face setups
(add-hook 'prog-mode-hook '+setup-prog-faces)
(add-hook 'text-mode-hook '+setup-text-faces)

;;; Load customizations
(+load-look-and-feel)

(provide 'init-look-and-feel)
;;; init-look-and-feel.el ends here
