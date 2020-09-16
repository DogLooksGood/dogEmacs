;;; -*- lexical-binding: t -*-

(defvar +dumped-load-path nil
  "Not nil when using dump.")

;;; Some setup for startup with dump
(when +dumped-load-path
  ;; Restore the load path
  (setq load-path +dumped-load-path)
  ;; Disable error message
  (setq warning-minimum-level :emergency)
  ;; Some shim code for tramp
  (defun tramp-file-name-method--cmacro (&rest args))
  (require 'tramp)
  (setq tramp-mode 1)
  ;; These two modes are disabled in pdump
  (global-font-lock-mode t)
  (transient-mark-mode t))

(unless +dumped-load-path
  ;; Load path for user config
  (setq +init-file (or load-file-name (buffer-file-name)))
  (setq +emacs-dir (file-name-directory +init-file))
  (add-to-list 'load-path (concat +emacs-dir "elisp/"))

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
  (require 'init-elisp)
  (require 'init-tab)
  (require 'init-look-and-feel)
  (require 'init-global)
  (require 'init-git)
  (require 'init-ivy)
  (require 'init-completion)
  (require 'init-paren)
  (require 'init-clojure)
  (require 'init-java)
  (require 'init-javascript)
  (require 'init-typescript)
  (require 'init-haskell)
  (require 'init-elixir)
  (require 'init-rust)
  (require 'init-conf)
  (require 'init-org)
  (require 'init-markdown)
  (require 'init-sql)
  (require 'init-html)
  (require 'init-edit)
  (require 'init-window)
  (require 'init-nav)
  (require 'init-project)
  (require 'init-visual)
  (require 'init-eshell)
  (require 'init-lsp)
  (require 'init-dired)
  (require 'init-ibuffer)
  (require 'init-snippet)
  (require 'init-latex)
  (require 'init-ebook)
  (require 'init-deft)
  (require 'init-docker)
  (require 'init-server)
  (require 'init-rime)
  (require 'init-telegram)
  (require 'init-restclient)
  (require 'init-tmux)
  (require 'init-meow)
  (require 'init-pass)
  (require 'init-dictionary)
  (require 'init-modeline)
  (require 'init-macos)
  (require 'init-work))

;; Proper GC after startup
(setq gc-cons-threshold (* 1024 128))
(put 'magit-edit-line-commit 'disabled nil)
