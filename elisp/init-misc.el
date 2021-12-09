(straight-use-package 'dumb-jump)
(straight-use-package 'highlight-numbers)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(autoload 'dump-jump-xref-active "dump-jump")
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

(require 'colorful)
(add-hook 'prog-mode-hook 'colorful-mode)

;; (require 'htab)
;; (htab-global-mode 1)
;; (add-to-list 'htab-ignore-commands 'meow-minibuffer-quit)
;; (global-set-key (kbd "<XF86Forward>") #'htab-next-buffer)
;; (global-set-key (kbd "<XF86Back>") #'htab-prev-buffer)

(defun +project-previous-buffer (arg)
  "Toggle to the previous buffer that belongs to current project
and don't shown in any window."
  (interactive "P")
  (unless arg
    (if-let ((pr (project-current)))
        (switch-to-buffer
         (->> (project--buffer-list pr)
              (--remove (or (minibufferp it)
                            (get-buffer-window-list it)))
              (car)))
      (mode-line-other-buffer))))

(provide 'init-misc)
