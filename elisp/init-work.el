
(defun +open-work-log ()
  (interactive)
  (let* ((hour (string-to-number (shell-command-to-string "echo -n $(date +%H)")))
         (date (shell-command-to-string "echo -n $(date +%m-%d-%Y)")))
    (find-file (expand-file-name (format "~/Note/%s.md" date)))))

(provide 'init-work)
