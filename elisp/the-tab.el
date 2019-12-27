;;; A customized tab keybinding.

(defun user/insert-tab ()
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively 'indent-region))
   ((string-match-p "^[ \t]*$"
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (point)))
    (call-interactively #'indent-for-tab-command))
   ((nth 3 (syntax-ppss))
    (paredit-forward-up))
   (t
    (company-complete-common-or-cycle))))

(defun user/normal-tab ()
  "Indent the current line or region, or toggle hideshow.
org-cycle in org-mode"
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((derived-mode-p 'org-mode)
    (org-cycle))
   ((memq indent-line-function
          '(indent-relative indent-relative-maybe))
    (hs-toggle-hiding))
   ((let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t))
      (call-interactively #'indent-for-tab-command)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (hs-toggle-hiding))))))

(bind-key "<tab>" 'user/insert-tab prog-mode-map)

(provide 'the-tab)
