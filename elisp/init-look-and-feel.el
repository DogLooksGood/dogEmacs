;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  mode line and themes.

(set-frame-parameter nil 'alpha '(100 . 100))

(defun +debug-load-theme ()
  (interactive)
  (let ((font "sarasa mono sc-11"))
  (add-to-list 'default-frame-alist (cons 'font font))
  (set-frame-font font nil t)
  (require 'storybook-theme)
  (load-theme 'storybook t)
  (set-face-attribute 'font-lock-function-name-face nil :height 140)))

(+debug-load-theme)

(bind-key "C-z" '+debug-load-theme)

(defvar-local +current-buffer-vc-path nil)

(defun +smart-file-name ()
  (if +current-buffer-vc-path
      +current-buffer-vc-path
    (if (not vc-mode)
        (buffer-name)
      (setq-local +current-buffer-vc-path (file-relative-name (buffer-file-name) (vc-root-dir)))
      +current-buffer-vc-path)))

(defun +project-name ()
  (if vc-mode
      (format "  { %s }" (vc-root-dir))
    ""))

;;; title line setup
(setq-default frame-title-format
              '("Emacs "
                (:eval (+project-name))))

;;; If we want hide the mode line
(setq +mini-mode-line t)

(defun +mode-base-info (format-string)
  "Return formatted string if there's still enough space."
  (let ((s (format-mode-line format-string)))
    (when (or (not mini-modeline--msg)
              (> (window-width) (+ 12 (string-width s) (string-width mini-modeline--msg))))
      s)))

(setq-default mode-line-format '((:eval (meow-indicator))
                                 " "
                                 (:eval (when rime-mode (concat  (rime-lighter) " ")))
                                 (:eval (+smart-file-name))
                                 " %* %m "
                                 (vc-mode vc-mode)))

;; Only show window divider when there's more than one window.
;; (defun +toggle-window-divider-and-border ()
;;   (unless (string-match-p ".*-posframe\\*" (buffer-name (current-buffer)))
;;     (if (> (count-windows) 1)
;;         (progn
;;           (window-divider-mode 1))
;;       (progn
;;         (window-divider-mode -1)))))

;; (add-hook 'window-configuration-change-hook #'+toggle-window-divider-and-border)

(provide 'init-look-and-feel)
