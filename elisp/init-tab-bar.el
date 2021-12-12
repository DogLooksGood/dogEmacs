;;; -*- lexical-binding: t -*-

(setq tab-bar-border nil
      tab-bar-close-button nil
      tab-bar-back-button nil
      tab-bar-new-button nil
      tab-bar-format '(tab-bar-format-tabs +tab-bar-right)
      tab-bar-tab-name-format-function '+tab-bar-tab-format-function)

(defun +tab-bar-right ()
  (let* ((p (cdr (project-current)))
         (vc (+vc-branch-name))
         (w (string-width (concat p " " vc))))
    (concat (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,w 1))))
            p
            " "
            vc)))

(defun +tab-bar-switch-project ()
  "Switch to project in a new tab, project name will be used as tab name.

No tab will created if the command is cancelled."
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

(global-set-key (kbd "C-x t .") #'tab-bar-rename-tab)
(global-set-key (kbd "C-x t l") #'+tab-bar-switch-project)
(global-set-key (kbd "<C-next>") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "<C-prior>") #'tab-bar-switch-to-prev-tab)

(tab-bar-mode 1)

(provide 'init-tab-bar)
