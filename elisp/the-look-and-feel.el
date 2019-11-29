;; Look & Feels

;; Transparency Setup

(defun user/set-alpha (alpha)
  (set-frame-parameter (selected-frame) 'alpha (cons alpha alpha))
  (add-to-list 'default-frame-alist (cons 'alpha (cons alpha alpha))))

;; Font Setup

;; | 为了中英文等宽的字体     |
;; | For mixed monospace font |

(when (display-graphic-p)
  (set-frame-font "fira emacs retina-10")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "wenquanyi zen hei" :size 36)))
  (user/set-alpha 100))

(setq overline-margin 0)

(bind-key "C-S-U" 'counsel-load-theme)

(defvar user/current-theme nil)
(defvar user/selected-themes nil)

(setq user/selected-themes
      '(github-theme
        zenburn-theme
        nimbus-theme))

(seq-doseq (p user/selected-themes)
  (unless (package-installed-p p)
    (package-install p)))

(defun user/use-theme (theme custom-faces)
  (disable-theme user/current-theme)
  (load-theme theme t)
  (apply 'custom-theme-set-faces
   theme
   custom-faces)
  (setq user/current-theme theme))

(setq user/themes
      '((zenburn
         ())
        (github
         ((cider-fringe-good-face ((t :foreground "#009966")))
          (cider-test-success-face ((t :foreground "black" :background "green")))
          (cider-test-error-face ((t :foreground "black" :background "MediumPurple1")))
          (cider-test-failure-face ((t :foreground "black" :background "red")))
          (ivy-minibuffer-match-face-4 ((t :foreground "white" :background "#c6a80f")))
          (ivy-minibuffer-match-face-3 ((t :foreground "white" :background "#3aa80f")))
          (ivy-minibuffer-match-face-2 ((t :foreground "white" :background "#099b85")))))
        (nimbus
         ())))

(defun user/toggle-theme (&optional theme-name)
  (interactive)
  (if theme-name
      (let ((theme (-find (lambda (x) (eq (car x) theme-name)) user/themes)))
        (when theme
          (apply 'user/use-theme theme)))
    (let* ((next-theme (->> (-drop-while (lambda (x) (not (eq (car x) user/current-theme)))
                                         user/themes)
                            (-second-item)))
           (theme (if next-theme next-theme
                    (car user/themes))))
      (apply 'user/use-theme theme))))

(bind-key "C-S-D" 'user/toggle-theme)

(user/toggle-theme 'github)

(custom-set-faces
 '(fringe ((t :background nil)))
 '(yas-field-highlight-face ((t :box "#777"))))

(provide 'the-look-and-feel)
