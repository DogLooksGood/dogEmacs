
(straight-use-package 'restclient)

(+pdump-packages 'restclient)

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

(provide 'init-restclient)
