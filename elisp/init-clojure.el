;;; -*- lexical-binding: t -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'clj-refactor)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)

(+pdump-packages 'clojure-mode
                 'cider
                 'clj-refactor
                 'flycheck
                 'flycheck-clj-kondo)

;;; clojure-mode

(setq
 clojure-toplevel-inside-comment-form t)

(autoload #'clojure-mode "clojure-mode")

(with-eval-after-load "clojure-mode"
  (require 'smartparens-clojure)
  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  (require 'init-clojure-highlight-fix)

  (dolist (f '(clj-refactor-mode flycheck-mode smartparens-mode smartparens-strict-mode))
    (add-hook 'clojure-mode-hook f))

  (define-key clojure-mode-map (kbd ";") '+lisp-semicolon)
  (define-key clojure-mode-map (kbd "C-c C-f") 'zprint)

  (require 'flycheck-clj-kondo))

(with-eval-after-load "flycheck"
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
  (setq flycheck-idle-change-delay 1))

;;; clj-refactor

(setq
 cljr-warn-on-eval t
 cljr-suppress-middleware-warnings t)

(autoload #'clj-refactor-mode "clj-refactor")

(with-eval-after-load "clj-refactor"
  (define-key clj-refactor-map (kbd "/") nil)
  (cljr-add-keybindings-with-prefix "C-c C-r"))

;;; cider

(defun +clojure-describe-spec ()
  (interactive)
  (when-let* ((code (thing-at-point 'symbol))
              (dict (cider-nrepl-sync-request:eval
                     code
                     (--find (eq (cider-connection-type-for-buffer)
                                 (cider-connection-type-for-buffer it))
                             (cider-connections))
                     (cider-ns-from-form (cider-ns-form))))
              (spec (-last-item dict)))
    (cider-browse-spec spec)))

(setq
 cider-font-lock-dynamically nil
 cider-font-lock-reader-conditionals t
 cider-use-fringe-indicators t
 cider-prompt-for-symbol nil
 cider-save-file-on-load t
 cider-enhanced-cljs-completion-p nil
 cider-offer-to-open-cljs-app-in-browser nil)

(autoload #'cider-jack-in "cider" nil t)
(autoload #'cider-jack-in-cljs "cider" nil t)
(autoload #'cider-jack-in-clj&cljs "cider" nil t)
(autoload #'cider "cider" nil t)

(with-eval-after-load "cider"
  (define-key cider-mode-map (kbd "C-c M-s") #'+clojure-describe-spec)
  (define-key cider-mode-map (kbd "C-c f") #'cider-pprint-eval-defun-at-point)
  (define-key clojure-mode-map (kbd "C-c C-i") 'cider-inspect-last-result))

;; zprint

(defun zprint (&optional is-interactive)
  "Reformat code using zprint.
If region is active, reformat it; otherwise reformat entire buffer.
When called interactively, or with prefix argument IS-INTERACTIVE,
show a buffer if the formatting fails"
  (interactive)
  (let* ((contents (buffer-string))
         (ln (line-number-at-pos (point) t))
         (col (- (point) (line-beginning-position)))
         (formatted-contents
          (with-temp-buffer
            (insert contents)
            (let ((retcode (call-process-region
                            (point-min)
                            (point-max)
                            "zprint"
                            t
                            (current-buffer)
                            nil)))
              (if (zerop retcode)
                  (buffer-string)
                (error "zprint failed: %s" (string-trim-right (buffer-string))))))))
    (erase-buffer)
    (insert formatted-contents)
    (goto-char (point-min))
    (forward-line (1- ln))
    (forward-char col)))

(provide 'init-clojure)
