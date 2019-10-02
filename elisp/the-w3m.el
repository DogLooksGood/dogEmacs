(use-package w3m
  :bind
  (:map
   w3m-mode-map
   ("n" . 'forward-line)
   ("p" . 'previous-line)))

(provide 'the-w3m)
