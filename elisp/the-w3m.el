(use-package w3m
  :bind
  (:map
   w3m-mode-map
   ("p" . 'previous-line)
   ("n" . 'forward-line)))


(provide 'the-w3m)
