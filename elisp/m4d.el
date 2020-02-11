;;; -*- lexical-binding: t -*-
(require 'multiple-cursors)

;;; Faces

(defface m4d-kmacro-indicator
  '((((class color) (background dark))
      (:inherit font-lock-string-face))
     (((class color) (background light))
      (:inherit font-lock-string-face)))
  "Kmacro indicator"
  :group 'm4d)

(defface m4d-visual-indicator
  '((((class color) (background dark))
      (:inherit default))
     (((class color) (background light))
      (:inherit default)))
  "Visual indicator"
  :group 'm4d)

(defface m4d-insert-indicator
  '((((class color) (background dark))
      (:inherit font-lock-function-name-face))
     (((class color) (background light))
      (:inherit font-lock-function-name-face)))
  "Insert indicator"
  :group 'm4d)

(defface m4d-motion-indicator
  '((((class color) (background dark))
      (:inherit font-lock-constant-face))
     (((class color) (background light))
      (:inherit font-lock-constant-face)))
  "Motion indicator"
  :group 'm4d)

;;; Custom Variables

(defvar m4d-insert-modal-hook nil
  "A hook runs when we enter the insert modal.")

(defvar m4d-normal-modal-hook nil
  "A hook runs when we enter the normal modal.")

(defvar m4d-motion-modal-hook nil
  "A hook runs when we enter the motion modal.")

(defvar m4d-disable-normal-mode-list nil
  "A list of modes should not enable normal mode.")
(setq m4d-disable-normal-mode-list
      '(magit-status-mode))

(defvar m4d-motion-mode-list nil
  "A list of modes should be treated as special mode .")
(setq m4d-motion-mode-list
      '(dired-mode
        ripgrep-search-mode
        help-mode
        compilation-mode
        eshell-mode))

(defvar m4d-enable-mode-list
  '(json-mode)
  "A list of major-modes where we should enable modal edit.")

;;; Internal Variables

(defvar m4d--position-record nil
  "The record of positions, car is point, cdr is mark.")

(defvar m4d--last-select nil
  "The last select behavior.")

(defvar m4d--selections nil
  "All selections")

(defvar m4d--leader-mode-keymaps nil
  "Leader keymaps used for major modes.")

;;; Define key helpers

(require 'm4d-util)
(require 'm4d-core)
(require 'm4d-kmacro)
(require 'm4d-keys)
(require 'm4d-helpers)
(require 'm4d-setup)

(defun m4d-indicator ()
  (interactive)
  (cond
   (god-local-mode
    (propertize "KMACRO" 'face 'm4d-kmacro-indicator))
   (m4d-normal-mode
    (propertize "NORMAL" 'face 'm4d-visual-indicator))
   ((m4d--should-enable-motion-p)
    (propertize "MOTION" 'face 'm4d-motion-indicator))
   (t
    (propertize "INSERT" 'face 'm4d-insert-indicator))))

;;;###autoload

(defun m4d--normal-init ()
  (m4d--mc-setup)
  (run-hooks 'm4d-normal-modal-hook)
  (when-let ((keymap (m4d--get-mode-leader-keymap major-mode t)))
    (local-set-key (kbd "M-SPC") keymap))
  (m4d--update-cursor-shape))

(defun m4d--motion-init ()
  (m4d--mc-setup)
  (run-hooks 'm4d-motion-modal-hook)
  (when-let ((keymap (m4d--get-mode-leader-keymap major-mode t)))
    (local-set-key (kbd "M-SPC") keymap))
  (m4d--update-cursor-shape))

(define-minor-mode m4d-kmacro-mode
  "m4d kmacro modal state."
  nil
  ""
  (when m4d-kmacro-mode
    (m4d--kmacro-init)))

;;;###autoload
(define-minor-mode m4d-normal-mode
  "m4d normal modal state."
  nil
  ""
  m4d-normal-keymap
  (when m4d-normal-mode
    (m4d--normal-init)))

;;;###autoload
(define-minor-mode m4d-motion-mode
  "m4d special mode"
  nil
  nil
  m4d-motion-keymap
  (when m4d-motion-mode
    (m4d--motion-init)))

;;;###autoload
(define-minor-mode m4d-mode
  "Modal for Dvorak."
  nil
  " M4D"
  m4d-keymap)

;;;###autoload
(define-global-minor-mode m4d-global-mode m4d-mode
  (lambda ()
    (add-hook 'post-command-hook #'m4d--update-cursor-shape t t)
    (m4d-mode 1)
    (when (m4d--should-enable)
      (m4d-normal-mode 1))
    (when (m4d--should-enable-motion-p)
      (m4d-motion-mode 1))))

(provide 'm4d)
