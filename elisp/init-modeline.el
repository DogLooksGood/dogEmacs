;;; -*- lexical-binding: t -*-
;;; A deadly simple mode line customization.

(defvar-local +current-buffer-vc-path nil)
(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path."
  (if +current-buffer-vc-path
      +current-buffer-vc-path
    (if (not vc-mode)
        (buffer-name)
      (setq-local +current-buffer-vc-path (file-relative-name (buffer-file-name) (vc-root-dir)))
      +current-buffer-vc-path)))

(defun +project-name ()
  "Get project name, which is used in title format."
  (if vc-mode
      (format "  { %s }" (vc-root-dir))
    ""))

;;; title line setup
(setq-default frame-title-format
              '("Emacs "
                (:eval (+project-name))))

(setq-default mode-line-format '((:eval (meow-indicator))
                                 " %l "
                                 (:eval (when rime-mode (concat  (rime-lighter) " ")))
                                 (:eval (+smart-file-name))
                                 " %* %m "
                                 (vc-mode vc-mode)))

(provide 'init-modeline)
;;; init-modeline.el ends here
