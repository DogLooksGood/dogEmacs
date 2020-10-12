;;; -*- lexical-binding: t -*-
;;; A customized tab keybinding.
;;; but currently, we need company to trigger on anytime.
(defun +insert-tab ()
  (interactive)
  (cond
   ((string-match-p "^[ \t]*$"
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (point)))
    (call-interactively #'indent-for-tab-command))
   (yas/minor-mode
    (let ((yas/fallback-behavior 'return-nil))
      (or (yas/expand)
          (company-complete-common))))
   (t
    (company-complete-common))))

(defun +normal-tab ()
  "Indent the current line or region, or toggle hideshow.
org-cycle in org-mode"
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((derived-mode-p 'org-mode)
    (org-cycle))
   ;; Format the top-level with a single command.
   (paredit-mode
    (call-interactively #'paredit-reindent-defun))
   (t
    (call-interactively #'indent-for-tab-command))))

(provide 'init-tab)
