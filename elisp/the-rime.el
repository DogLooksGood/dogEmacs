
(defun user/rime-predicate-not-in-insert-mode ()
  (or m4d-keypad-mode
      m4d-normal-mode
      m4d-motion-mode))

(defun user/rime-predicate-is-back-quote-or-tilde ()
  (or (equal rime--current-input-key ?`)
      (equal rime--current-input-key ?~)))

(use-package rime
  :bind
  (:map
   rime-active-mode-map
   ("M-j" . 'rime-inline-ascii)
   :map rime-mode-map
   ("C-$" . 'rime-send-keybinding)
   ("M-j" . 'rime-force-enable)
   ("C-SPC" . 'toggle-input-method))
  :custom
  ((rime-disable-predicates '(user/rime-predicate-not-in-insert-mode
                              rime-predicate-prog-in-code-p
                              rime-predicate-after-alphabet-char-p))
   (rime-inline-predicates '(rime-predicate-space-after-cc-p
                             user/rime-predicate-is-back-quote-or-tilde
                             rime-predicate-current-uppercase-letter-p))
   (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
   (default-input-method "rime")
   (rime-cursor "˰")
   (rime-show-candidate 'posframe)))

;;; Used for package developing

;; (setq rime-librime-root "~/.emacs.d/librime/dist")
;; (add-to-list 'load-path "~/develop/emacs-rime/")
;; (require 'rime)
;; (setq default-input-method "rime")
;; (setq rime-show-candidate 'posframe)
;; (setq rime-cursor "˰")
;; (bind-key "M-j" 'rime-force-enable rime-mode-map)
;; (bind-key "M-j" 'rime-inline-ascii rime-active-mode-map)
;; (bind-key "C-SPC" 'toggle-input-method)
;; (bind-key "C-$" 'rime-send-keybinding rime-mode-map)
;; (setq rime-disable-predicates '(user/rime-predicate-not-in-insert-mode
;;                                 rime-predicate-prog-in-code-p
;;                                 rime-predicate-after-alphabet-char-p))
;; (setq rime-inline-predicates '(rime-predicate-space-after-cc-p
;;                                rime-predicate-current-uppercase-letter-p))
;; (add-hook 'change-major-mode-after-body-hook 'deactivate-input-method)
;; (setq rime-posframe-properties
;;  (list :background-color "#333333"
;;        :foreground-color "#dcdccc"
;;        :font "unifont"
;;        :internal-border-width 10))

(provide 'the-rime)
