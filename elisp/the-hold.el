(use-package shiftless
  :quelpa (shiftless
           ;; :fetcher github
           ;; :repo "DogLooksGood/shiftless.el"
           :fetcher file
           :path "~/develop/shiftless.el")
  :init
  (setq shiftless-delay 0.185
        shiftless-interval 0.04)
  (shiftless-mode 1)
  :config
  (shiftless-use-layout-dvp))

(provide 'the-hold)
