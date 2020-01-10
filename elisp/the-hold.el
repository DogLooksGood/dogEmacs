(use-package shiftless
  :quelpa (shiftless
           :fetcher github
           :repo "DogLooksGood/shiftless.el")
  :init
  (setq shiftless-delay 0.3
        shiftless-interval 0.1)
  (shiftless-mode 1)
  :config
  (shiftless-use-layout-dvp))

(advice-add 'sp--post-self-insert-hook-handler :around #'shiftless--prevent-advice)

(provide 'the-hold)
