;; Look & Feels

;; Font Setup

;; Try set it in ~/.Xresources
(progn
  (setq my-font "consolas-14")
  (setq my-font "等距更纱黑体 cl-14")
  (set-default-font my-font)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (set-face-attribute 'default t :font my-font))

;; For Org-mode

(add-hook 'org-mode-hook
          (lambda ()
            (face-remap-add-relative
             'default :family "等距更纱黑体 CL")))

;; Transparency Setup

(defun user/set-alpha (alpha)
  (set-frame-parameter (selected-frame) 'alpha (cons alpha alpha))
  (add-to-list 'default-frame-alist (cons 'alpha (cons alpha alpha))))

(user/set-alpha 84)

;;; themes

(let ((pkgs '(leuven-theme zenburn-theme nimbus-theme)))
  (mapcar (lambda (pkg)
            (unless (package-installed-p pkg)
              (package-install pkg)))
          pkgs))

(require 'vanilla-theme)

(enable-theme 'vanilla)

;;; theme customize

(defun user/zenburn-theme-setup ()
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   '(leerzeichen ((t :foreground "#3f3f3f")))
   '(show-paren-mismatch ((t :background "#aa3333" :foreground "#ffffff")))
   '(highlight-symbol-face ((t :underline "#999999")))))

(defun user/leuven-theme-setup ()
  (load-theme 'leuven t)
  (custom-theme-set-faces
   'leuven
   '(font-lock-keyword-face ((nil :italic t)))
   '(clojure-keyword-face ((t :foreground "#006ef0")))
   '(default ((t :background "#efefef")))))

(defun user/nimbus-theme-setup ()
  (load-theme 'nimbus t)
  (custom-theme-set-faces
   'nimbus
   '(fringe ((t :background nil)))
   '(leerzeichen ((t :foreground "#1a1a1a")))
   '(show-paren-match ((t :bold t :foreground "#fffe0a")))
   '(show-paren-mismatch ((t :background "#aa3333" :foreground "#ffffff")))
   '(highlight-symbol-face ((t :background "#2a322a")))))

(defun user/select-theme ()
  (interactive)
  (let ((theme (ivy-read "Switch to theme:"
                         '("zenburn" "leuven" "nimbus" "vanilla"))))
    (mapcar (lambda (theme) (disable-theme theme))
            custom-enabled-themes)
    (message theme)
    (cond
      ((equal theme "zenburn") (user/zenburn-theme-setup))
      ((equal theme "leuven") (user/leuven-theme-setup))
      ((equal theme "nimbus") (user/nimbus-theme-setup))
      ((equal theme "vanilla") (enable-theme 'vanilla)))))

(bind-key "C-S-c" 'user/select-theme)

(provide 'the-look-and-feel)
