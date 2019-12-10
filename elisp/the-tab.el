;;; A customized tab keybinding.

(defun user/insert-tab ()
  (interactive)
  (if (nth 3 (syntax-ppss))
      (paredit-forward-up)
    (company-indent-or-complete-common)))

(defun user/normal-tab ()
  "Indent the current line or region, or toggle hideshow."
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
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
