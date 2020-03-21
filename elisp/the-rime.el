
(defun user/rime-not-in-insert-mode ()
  (or m4d-keypad-mode
      m4d-normal-mode
      m4d-motion-mode))

;; (use-package rime
;;   ;; :quelpa (rime
;;   ;;          :files ("rime.el" "Makefile" "lib.c")
;;   ;;          :fetcher file
;;   ;;          :path "~/develop/emacs-rime/")
;;   :quelpa (rime
;;            :fetcher github
;;            :files ("rime.el" "Makefile" "lib.c")
;;            :repo "DogLooksGood/emacs-rime")
;;   :bind
;;   (:map
;;    rime-mode-map
;;    ("C-$" . 'rime-send-keybinding)
;;    ("M-j" . 'rime-force-enable)
;;    ("C-SPC" . 'toggle-input-method))
;;   :custom
;;   ((rime-disable-predicates '(user/rime-not-in-insert-mode
;;                               rime-predicate-prog-in-code-p
;;                               rime-predicate-after-alphabet-char-p))
;;    (rime-translate-keybindings '("C-`" "C-f" "C-b" "C-n" "C-p" "C-g"))
;;    (default-input-method "rime")
;;    (rime-cursor "˰")
;;    (rime-show-candidate 'posframe)
;;    (rime-posframe-properties
;;     (list :background-color "#333333"
;;           :foreground-color "#dcdccc"
;;           :font "sarasa ui sc"
;;           :internal-border-width 10))))


;;; Used for package developing

(setq rime-librime-root "~/.emacs.d/librime/dist")
(add-to-list 'load-path "~/develop/emacs-rime/")
(require 'rime)
(setq default-input-method "rime")
(setq rime-show-candidate 'posframe)
(bind-key "M-j" 'rime-force-enable rime-mode-map)
(bind-key "M-j" 'rime-inline-ascii rime-active-mode-map)
(bind-key "C-SPC" 'toggle-input-method)
(bind-key "C-$" 'rime-send-keybinding rime-mode-map)
(setq rime-disable-predicates '(user/rime-not-in-insert-mode
                                rime-predicate-prog-in-code-p
                                rime-predicate-after-alphabet-char-p))
(setq rime-inline-predicates '(rime-predicate-space-after-cc-p))

(setq rime-posframe-properties
 (list :background-color "#333333"
       :foreground-color "#dcdccc"
       ;; :font "sarasa ui sc"
       :internal-border-width 10))

(provide 'the-rime)
