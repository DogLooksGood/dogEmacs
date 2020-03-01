;;; -*- lexical-binding: t -*-
(require 'multiple-cursors)

;;; Faces

(defface m4d-keypad-indicator
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

(defvar m4d-dvorak-layout-type 'programmer
  "Can be `programmer' or `simplified'.")

(defvar m4d-insert-modal-hook nil
  "A hook runs when we enter the insert modal.")

(defvar m4d-insert-exit-hook nil
  "A hook runs when we exit the insert modal.")

(defvar m4d-normal-modal-hook nil
  "A hook runs when we enter the normal modal.")

(defvar m4d-motion-modal-hook nil
  "A hook runs when we enter the motion modal.")

(defvar m4d-motion-mode-list nil
  "A list of modes should enable motion mode.")
(setq m4d-motion-mode-list
      '(dired-mode
        ivy-occur-grep-mode
        ripgrep-search-mode
        help-mode
        compilation-mode
        dired-sidebar-mode))

(defvar m4d-normal-mode-list nil
  "A list of modes should enable normal mode.")
(setq m4d-normal-mode-list
      '(cider-repl-mode
        eshell-mode
        vterm-mode
        json-mode
        wdired-mode
        deft-mode))

;;; Internal Variables

(defvar m4d--stick-modes nil
  "In these modes, don't auto switch mode.")
(setq m4d--stick-modes
      '(ripgrep-search-mode
        ivy-occur-grep-mode))

(defvar m4d--keymap-loaded nil
  "If keymap is loaded in this buffer.")
(make-variable-buffer-local 'm4d--keymap-loaded)

(defvar m4d--space-command nil
  "SPC mapping in this buffer.")
(make-variable-buffer-local 'm4d--space-command)

(defvar m4d--position-record nil
  "The record of positions, car is point, cdr is mark.")

(defvar m4d--last-select nil
  "The last select behavior.")

(defvar m4d--selections nil
  "All selections")

(defvar m4d--leader-mode-keymaps nil
  "Leader keymaps used for major modes.")

(defvar m4d--keypad-keys nil
  "Current keys in kmacro mode.")

;;; Define key helpers

(require 'm4d-util)
(require 'm4d-core)
(require 'm4d-keypad)
(require 'm4d-keys)
(require 'm4d-helpers)
(require 'm4d-esc)
(require 'm4d-setup)

(defun m4d-indicator ()
  (interactive)
  (cond
   (m4d-keypad-mode
    (concat
     (propertize "KEYPAD [" 'face 'm4d-keypad-indicator)
     (m4d--keypad-format-keys)
     (propertize "] " 'face 'm4d-keypad-indicator)))
   (m4d-normal-mode
    (propertize
     (if (m4d--direction-right-p)
         "NORMAL"
       "NORMAL«")
     'face 'm4d-visual-indicator))
   (m4d-motion-mode
    (propertize "MOTION" 'face 'm4d-motion-indicator))
   (m4d-insert-mode
    (cond
     ((and buffer-read-only (not (equal major-mode 'vterm-mode)))
      (propertize "READONLY" 'face 'm4d-insert-indicator))
     ((bound-and-true-p overwrite-mode)
      (propertize "OVERWRITE" 'face 'm4d-insert-indicator))
     (t (propertize "INSERT" 'face 'm4d-insert-indicator))))
   (t "")))

;;;###autoload

(defun m4d--normal-init ()
  ;; (message "%s normal init" major-mode)
  (run-hooks 'm4d-normal-modal-hook)
  (unless m4d--keymap-loaded
    (let ((keymap (m4d--get-mode-leader-keymap major-mode t)))
      (define-key m4d-normal-keymap (kbd "SPC") keymap)
      (setq m4d--keymap-loaded t)))
  (m4d--update-cursor-shape))

(defun m4d--motion-init ()
  ;; (message "%s motion init" major-mode)
  (run-hooks 'm4d-motion-modal-hook)
  (unless m4d--keymap-loaded
    (let ((keymap (m4d--get-mode-leader-keymap major-mode t)))
      (define-key m4d-motion-keymap (kbd "SPC") keymap))
    (setq m4d--keymap-loaded t))
  (m4d--update-cursor-shape))

(defun m4d--keypad-init ()
  (run-hooks 'm4d-keypad-mode-hook)
  (setq m4d--keypad-keys nil
        m4d--use-literal nil
        m4d--use-meta nil))

(defun m4d--keypad-uninit ())

(defun m4d--insert-init ()
  (run-hooks 'm4d-insert-mode-hook))

(defun m4d--insert-uninit ()
  (run-hooks 'm4d-insert-exit-hook))

(defun m4d--mode-init ()
  (when (m4d--should-enable-motion-p)
    (unless m4d--space-command
      (let ((cmd (key-binding (read-kbd-macro "SPC"))))
        (when (and (commandp cmd)
                   (not (equal cmd 'undefined)))
          (setq-local m4d--space-command cmd))))))

(define-minor-mode m4d-insert-mode
  "m4d insert modal state."
  nil
  "[I]"
  m4d-insert-keymap
  (if m4d-insert-mode
      (m4d--insert-init)
    (m4d--insert-uninit)))

;;;###autoload
(define-minor-mode m4d-normal-mode
  "m4d normal modal state."
  nil
  "[N]"
  m4d-normal-keymap
  (when m4d-normal-mode
    (m4d--normal-init)))

;;;###autoload
(define-minor-mode m4d-motion-mode
  "m4d special mode"
  nil
  "[M]"
  m4d-motion-keymap
  (when m4d-motion-mode
    (m4d--motion-init)))

;;;###autoload
(define-minor-mode m4d-keypad-mode
  "m4d kmacro mode"
  nil
  "[K]"
  m4d-keypad-keymap
  (if m4d-keypad-mode
      (m4d--keypad-init)
    (m4d--keypad-uninit)))

;;;###autoload
(define-minor-mode m4d-mode
  "Modal for Dvorak."
  nil
  ""
  m4d-keymap
  (when m4d-mode
    (m4d--mode-init)))

;;;###autoload
(define-global-minor-mode m4d-global-mode m4d-mode
  (lambda ()
    (add-hook 'post-command-hook #'m4d--post-command-hook-function t t)
    (unless (minibufferp) (m4d-mode 1))
    (when (m4d--should-enable-normal-p)
      (m4d--switch-modal 'normal))
    (when (m4d--should-enable-motion-p)
      (m4d--switch-modal 'motion))))

(provide 'm4d)
