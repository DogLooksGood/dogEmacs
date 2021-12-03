;;; -*- lexical-binding: t -*-

(straight-use-package 'htmlize)
(straight-use-package 'org-roam)
(straight-use-package 'ob-restclient)
(straight-use-package 'ox-gfm)

;;; Latex support
;;; install latex with
;;; pacman -S texlive-bin texlive-most
;;; install xdot
;;; pacman -S xdot

;;; Update latex options after change theme.

(defun +org-update-latex-option-by-theme (theme)
  (when (bound-and-true-p org-format-latex-options)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :theme theme))))

(add-hook '+after-change-theme-hook '+org-update-latex-option-by-theme)

;;; Org babel

(defun +org-redisplay-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(defun +org-babel-setup ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook '+org-redisplay-inline-images))

;;; org-mode

(setq org-html-checkbox-type 'unicode)

(defun +org-init ()
  (variable-pitch-mode 1))

(with-eval-after-load  "org"
  ;; (define-key org-mode-map (kbd "<f8>") 'org-latex-auto-toggle)
  (define-key org-mode-map (kbd "<f5>") 'visible-mode)
  (require 'ox-gfm nil t)

  (require 'org-tempo)
  (+org-babel-setup)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0)
        org-agenda-files '("~/Org")
        org-hide-emphasis-markers t)
  (custom-set-faces
   '(org-table ((t :inherit 'fixed-pitch)))
   '(org-code ((t :inherit 'fixed-pitch)))
   '(org-block ((t :inherit 'fixed-pitch)))
   '(org-checkbox ((t :inherit 'fixed-pitch))))

  (add-hook 'org-mode-hook #'+org-init)
  (require 'ob)
  (require 'ob-dot)
  (require 'ob-restclient)
  (require 'ob-clojure)
  (require 'ob-js))

(with-eval-after-load "ob"
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)
     (restclient . t)
     (python . t)
     (clojure . t)
     (R . t)
     (shell . t))))

;;; org-roam

(setq
 org-roam-v2-ack t

 org-roam-directory
 (let ((p (expand-file-name "~/Org")))
   (unless (file-directory-p p) (make-directory p))
   p))

(with-eval-after-load "org-roam"
  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (org-roam-setup)
  (require 'org-roam-protocol))

(global-set-key (kbd "C-x M-n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-x M-n f") 'org-roam-node-find)
(global-set-key (kbd "C-x M-n g") 'org-roam-graph)
(global-set-key (kbd "C-x M-n i") 'org-roam-node-insert)
(global-set-key (kbd "C-x M-n c") 'org-roam-capture)
(global-set-key (kbd "C-x M-n s") 'org-roam-db-sync)

(autoload #'org-roam-capture "org-roam" nil t)
(autoload #'org-roam-node-insert "org-roam" nil t)
(autoload #'org-roam-node-find "org-roam" nil t)
(autoload #'org-roam-db-sync "org-roam" nil t)
(autoload #'org-roam-buffer-toggle "org-roam" nil t)

(provide 'init-org)
