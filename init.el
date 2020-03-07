(defvar user/launch-time (current-time))
(defvar user/dumped-load-path nil)
(when user/dumped-load-path
  (setq load-path user/dumped-load-path))

(setq user/init-file (or load-file-name (buffer-file-name)))
(setq user/emacs-dir (file-name-directory user/init-file))
(add-to-list 'load-path (concat user/emacs-dir "elisp/"))

(require 'package)

(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))

(setq package-selected-packages
      '(use-package quelpa-use-package company))

(if user/dumped-load-path
    (progn
      (global-font-lock-mode t)
      (transient-mark-mode t))
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package t)))

(setq quelpa-checkout-melpa-p nil)
(setq quelpa-update-melpa-p nil)
(setq use-package-always-ensure t)
(setq use-package-always-demand t)

;; Requires
(require 'use-package)
(require 'quelpa-use-package)
(quelpa-use-package-activate-advice)

(require 'the-elisp)
(require 'the-tab)
(require 'the-look-and-feel)
(require 'the-global)
(require 'the-git)
(require 'the-ivy)
(require 'the-prog)
(require 'the-completion)
(require 'the-lisp)
(require 'the-clojure)
(require 'the-java)
(require 'the-javascript)
(require 'the-haskell)
(require 'the-golang)
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
;; (require 'the-vterm)
(require 'the-rime)
(require 'the-wechat-mini)

(setq gc-cons-threshold (* 16 1024 1024))

(message "Emacs is ready, startup cost: %.3f seconds." (time-to-seconds (time-since user/launch-time)))
(setq user/launch-time nil)
