;;; -*- lexical-binding: t -*-

(setq tab-bar-border 0
      tab-bar-close-button nil
      tab-bar-back-button nil
      tab-bar-new-button nil
      tab-bar-format '(tab-bar-format-tabs)
      tab-bar-tab-name-format-function '+tab-bar-tab-format-function)

(defun +switch-project-in-new-tab ()
  (interactive)
  (let (succ)
    (unwind-protect
        (progn
          (tab-bar-new-tab)
          (call-interactively #'project-switch-project)
          (when-let ((proj (project-current)))
            (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
            (setq succ t)))
      (unless succ
        (tab-bar-close-tab)))))

(defun +tab-bar-tab-format-function (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (concat
     (propertize (concat
                  " "
                  (alist-get 'name tab)
                  " ")
                 'face
                 (funcall tab-bar-tab-face-function tab))
     " ")))

(global-set-key [remap project-switch-project] #'+switch-project-in-new-tab)

(tab-bar-mode 1)

(provide 'init-tab-bar)
