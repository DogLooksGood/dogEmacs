;;; -*- lexical-binding: t -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'clj-refactor)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)
(straight-use-package '(zprint :host github :repo "DogLooksGood/zprint.el"))

;;; clojure-mode

(setq clojure-toplevel-inside-comment-form t)

(autoload #'clojure-mode "clojure-mode")

(with-eval-after-load "clojure-mode"
  (require 'smartparens-clojure)
  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  (require 'init-clojure-highlight-fix)

  (require 'zprint)
  (require 'flycheck-clj-kondo)

  (define-clojure-indent
    (re-frame/reg-event-fx :defn)
    (re-frame/reg-event-db :defn)
    (re-frame/reg-sub :defn)
    (re-frame/reg-fx :defn))

  (dolist (f '(clj-refactor-mode flycheck-mode smartparens-mode smartparens-strict-mode))
    (add-hook 'clojure-mode-hook f))

  (define-key clojure-mode-map (kbd ";") '+lisp-semicolon)
  (define-key clojure-mode-map (kbd "C-c C-f") 'zprint))

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
  (define-key cider-stacktrace-mode-map (kbd "n") #'next-line)
  (define-key cider-stacktrace-mode-map (kbd "p") #'previous-line)
  (define-key cider-stacktrace-mode-map (kbd "P") #'cider-stacktrace-show-only-project)
  (define-key cider-mode-map (kbd "C-c M-s") #'+clojure-describe-spec)
  (define-key cider-mode-map (kbd "C-c f") #'cider-pprint-eval-defun-at-point)
  (define-key cider-mode-map (kbd "C-c C-f") 'nil)
  (define-key clojure-mode-map (kbd "C-c C-i") 'cider-inspect-last-result))

(provide 'init-clojure)
