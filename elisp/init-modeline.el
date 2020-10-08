;;; -*- lexical-binding: t -*-
;;; A deadly simple mode line customization.

(defvar-local +smart-file-name-cache nil
  "Cache for the smart file name of current buffer.")

(defvar-local +project-name-cache nil
  "Cache for current project name.")

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (cond
   (+smart-file-name-cache +smart-file-name-cache)
   ((and (buffer-file-name (current-buffer))
         (project-current))
    (setq-local +smart-file-name-cache
          (file-relative-name
           (buffer-file-name (current-buffer))
           (project-root (project-current)))))
   (t (setq-local +smart-file-name-cache (buffer-name)))))

(defun +project-name ()
  "Get project name, which is used in title format."
  (cond
   (+project-name-cache +project-name-cache)
   ((project-current)
    (setq-local +project-name-cache
          (format " : %s " (project-root (project-current)))))
   (t (setq-local +project-name-cache ""))))

(defun +mc-indicator ()
  "Display the number of cursors."
  (unless (= 1 (mc/num-cursors))
    (format "cur:%d" (mc/num-cursors))))

;;; title line setup
(setq-default frame-title-format '("Emacs" (:eval (+project-name))))

(setq-default mode-line-format '((:eval (meow-minimal-indicator))
                                 (:eval (+mc-indicator))
                                 "%l:%C "
                                 (:eval (when rime-mode (concat (rime-lighter) " ")))
                                 (:eval (+smart-file-name))
                                 "%* %m "
                                 (vc-mode vc-mode)
                                 ""))

(provide 'init-modeline)
;;; init-modeline.el ends here
