;;; -*- lexical-binding: t -*-

(defvar user/dumped-load-path nil
  "Not nil when using dump.")

;;; Some setup for startup with dump
(when user/dumped-load-path
  ;; Restore the load path
  (setq load-path user/dumped-load-path)
  ;; Disable error message
  (setq warning-minimum-level :emergency)
  ;; Some shim code for tramp
  (defun tramp-file-name-method--cmacro (&rest args))
  (require 'tramp)
  (setq tramp-mode 1)
  ;; These two modes are disabled in pdump
  (global-font-lock-mode t)
  (transient-mark-mode t))

(unless user/dumped-load-path
  ;; Load path for user config
  (setq user/init-file (or load-file-name (buffer-file-name)))
  (setq user/emacs-dir (file-name-directory user/init-file))
  (add-to-list 'load-path (concat user/emacs-dir "elisp/"))

  ;; Package & use-package & Quelpa initialize
  (require 'package)
  (setq package-archives
        '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
          ("melpa" . "http://elpa.emacs-china.org/melpa/")
          ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
  (package-initialize)
  (setq package-selected-packages
        '(use-package quelpa-use-package))
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (when (and (assq package package-archive-contents)
               (not (package-installed-p package)))
      (package-install package t)))
  (setq quelpa-checkout-melpa-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq use-package-always-demand t)
  (setq use-package-always-ensure t)
  (require 'use-package)
  (require 'quelpa-use-package)
  (quelpa-use-package-activate-advice)

  ;; Require modules
  (require 'the-elisp)
  (require 'the-tab)
  (require 'the-look-and-feel)
  (require 'the-global)
  (require 'the-git)
  (require 'the-ivy)
  (require 'the-completion)
  (require 'the-paren)
  (require 'the-clojure)
  (require 'the-java)
  (require 'the-javascript)
  (require 'the-typescript)
  (require 'the-haskell)
  (require 'the-rust)
  (require 'the-m4d)
  (require 'the-sql)
  (require 'the-html)
  (require 'the-edit)
  (require 'the-window)
  (require 'the-nav)
  (require 'the-project)
  (require 'the-visual)
  (require 'the-eshell)
  (require 'the-lsp)
  (require 'the-dired)
  (require 'the-conf)
  (require 'the-org)
  (require 'the-snippet)
  (require 'the-latex)
  (require 'the-ebook)
  (require 'the-deft)
  (require 'the-hold)
  (require 'the-docker)
  (require 'the-server)
  (require 'the-rime)
  (require 'the-telegram)
  (require 'the-wechat-mini))

;; Proper GC
(setq gc-cons-threshold (* 1024 128))
