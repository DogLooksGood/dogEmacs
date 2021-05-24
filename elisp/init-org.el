;;; -*- lexical-binding: t -*-

(straight-use-package 'htmlize)
(straight-use-package 'org-roam)
(straight-use-package 'org-roam-server)
(straight-use-package 'org-superstar)
(straight-use-package 'ob-restclient)
(straight-use-package '(org-html-themify
                        :type git
                        :host github
                        :repo "DogLooksGood/org-html-themify"
                        :files ("*.el" "*.js" "*.css")))

(+pdump-packages 'htmlize
                 'org
                 'org-roam
                 'org-roam-server
                 'org-superstar
                 'ob-restclient
                 'org-html-themify)

;;; Latex support
;;; install latex with
;;; pacman -S texlive-bin texlive-most
;;; install xdot
;;; pacman -S xdot

(defvar-local +org-last-in-latex nil)

(defun +org-post-command-hook ()
  (ignore-errors
    (let ((in-latex (and (derived-mode-p  'org-mode)
                         (or (org-inside-LaTeX-fragment-p)
                             (org-inside-latex-macro-p)))))
      (if (and +org-last-in-latex (not in-latex))
          (progn (org-latex-preview)
                 (setq +org-last-in-latex nil)))

      (when-let ((ovs (overlays-at (point))))
        (when (->> ovs
                   (--map (overlay-get it 'org-overlay-type))
                   (--filter (equal it 'org-latex-overlay)))
          (org-latex-preview)
          (setq +org-last-in-latex t)))

      (when in-latex
        (setq +org-last-in-latex t)))))

(define-minor-mode org-latex-auto-toggle
  "Auto toggle latex overlay when cursor enter/leave."
  :init-value nil
  :keymap nil
  :lighter nil
  (if org-latex-auto-toggle
      (add-hook 'post-command-hook '+org-post-command-hook nil t)
    (remove-hook 'post-command-hook '+org-post-command-hook t)))

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

(with-eval-after-load  "org"
  (define-key org-mode-map (kbd "<f8>") 'org-latex-auto-toggle)
  (require 'org-tempo)
  (+org-babel-setup)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0)
        org-agenda-files '("~/Org"))
  (custom-set-faces
   '(org-table ((t :inherit 'fixed-pitch)))
   '(org-code ((t :inherit 'fixed-pitch)))
   '(org-block ((t :inherit 'fixed-pitch)))
   '(org-checkbox ((t :inherit 'fixed-pitch))))

  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (require 'ob)
  (require 'ob-dot)
  (require 'ob-restclient)
  (require 'ob-clojure))

(with-eval-after-load "ob"
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)
     (restclient . t)
     (python . t)
     (clojure . t))))

;;; org-roam

(setq
 org-roam-directory
 (let ((p (expand-file-name "~/Org")))
   (unless (file-directory-p p) (make-directory p))
   p)

 org-roam-capture-templates
 '(("d" "default" plain (function org-roam--capture-get-point)
    "%?"
    :file-name "%<%Y%m%d%H%M%S>-${slug}"
    :head "#+title: ${title}\n"
    :unnarrowed t))

 org-roam-buffer-window-parameters '((no-delete-other-windows . t)))

(with-eval-after-load "org-roam"
  (define-key org-roam-mode-map (kbd "C-x C-r l") 'org-roam)
  (define-key org-roam-mode-map (kbd "C-x C-r f") 'org-roam-find-file)
  (define-key org-roam-mode-map (kbd "C-x C-r g") 'org-roam-graph)
  (define-key org-roam-mode-map (kbd "C-x C-r c") 'org-roam-db-build-cache)

  (define-key org-mode-map (kbd "<f7>") 'org-roam-insert)
  (define-key org-mode-map (kbd "C-x C-r i") 'org-roam-insert)
  (define-key org-mode-map (kbd "C-x C-r I") 'org-roam-insert-immediate)

  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (require 'org-roam-protocol))

;;; org-html-themify

(setq
 org-html-themify-themes '((dark . tao-yin)
                           (light . tao-yang)))

(autoload #'org-html-themify-mode "org-html-themify")

(add-hook 'org-mode-hook 'org-html-themify-mode)

(provide 'init-org)
