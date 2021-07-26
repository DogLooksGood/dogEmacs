;; -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)
(straight-use-package 'cargo)

(defun +rust-whitespace ()
  (interactive)
  (if (or (+in-string-p)
          (+in-comment-p))
      (call-interactively #'self-insert-command)
    (if (equal 59 (char-before))
        (progn
          (delete-char -1)
          (self-insert-command 1 ?:))
      (call-interactively #'self-insert-command))))

(defun +rust-lessthan ()
  (interactive)
  (if (or (+in-string-p)
          (+in-comment-p))
      (call-interactively #'self-insert-command)
    (if (equal 32 (char-before))
        (self-insert-command 1 ?<)
      (call-interactively #'self-insert-command))))

(defun +rust-semicolon ()
  (interactive)
  (cond
   ((or (+in-string-p)
        (+in-comment-p))
    (call-interactively #'self-insert-command))

   ((equal 58 (char-before))
    (self-insert-command 1 ?:))

   ((equal 59 (char-before))
    (progn
      (delete-char -1)
      (insert "::")))

   ((not (equal (point) (line-end-position)))
    (self-insert-command 1 ?:))

   (t (call-interactively #'self-insert-command))))

(defun +rust-minus ()
  "Will insert a minus if we are after whitespace and not at the indentation,otherwise will insert a underscore."
  (interactive)
  (if (and (or (+in-string-p)
               (+in-comment-p)
               (and (equal 32 (char-before))
		    (let ((pos (point)))
		      (not (equal pos
				  (save-mark-and-excursion
				    (back-to-indentation)
				    (point))))))))
      (call-interactively #'self-insert-command)
    (self-insert-command 1 ?_)))

;;; rust-mode

(autoload #'rust-mode "rust-mode")

(with-eval-after-load "rust-mode"
  (define-key rust-mode-map (kbd "-") #'+rust-minus)
  (define-key rust-mode-map (kbd "<") #'+rust-lessthan)
  (define-key rust-mode-map (kbd "SPC") #'+rust-whitespace)
  (define-key rust-mode-map (kbd ";") #'+rust-semicolon)
  (require 'smartparens-rust)
  (add-hook 'rust-mode-hook 'smartparens-mode)
  (add-hook 'rust-mode-hook 'eglot-ensure))

;;; cargo

(autoload #'cargo-process-run "cargo" nil t)
(autoload #'cargo-process-current-test "cargo" nil t)
(autoload #'cargo-process-current-file-tests "cargo" nil t)
(autoload #'cargo-process-test "cargo" nil t)
(autoload #'cargo-process-check "cargo" nil t)

(with-eval-after-load "cargo"
  (define-key rust-mode-map (kbd "C-c C-c") #'cargo-process-run)
  (define-key rust-mode-map (kbd "C-c C-t t") #'cargo-process-current-test)
  (define-key rust-mode-map (kbd "C-c C-t f") #'cargo-process-current-file-tests)
  (define-key rust-mode-map (kbd "C-c C-t p") #'cargo-process-test)
  (define-key rust-mode-map (kbd "C-c C-k") #'cargo-process-check))

(provide 'init-rust)
