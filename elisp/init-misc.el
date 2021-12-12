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

(defun +project-previous-buffer (arg)
  "Toggle to the previous buffer that belongs to current project
and don't shown in any window."
  (interactive "P")
  (unless arg
    (when-let ((pr (project-current)))
      (switch-to-buffer
       (->> (project--buffer-list pr)
         (--remove (or (minibufferp it)
                       (get-buffer-window-list it)))
         (car))))))

;; info
(require 'view)
(define-key Info-mode-map [remap scroll-up-command] #'View-scroll-half-page-forward)
(define-key Info-mode-map [remap scroll-down-command] #'View-scroll-half-page-backward)

;;; embark
(autoload 'embark-act "embark" nil t)

(global-set-key (kbd "<f2>") #'embark-act)

(with-eval-after-load "embark"
  (setq embark-mixed-indicator-delay 0)
  (define-key embark-meta-map (kbd "<escape>") #'keyboard-escape-quit))

(provide 'init-misc)
