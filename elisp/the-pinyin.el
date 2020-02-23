(use-package pinyinlib)

(defun re-builder-pinyin (str)
  (or (pinyin-to-utf8 str)
      (ivy--regex-plus str)
      (ivy--regex-ignore-order)))

(setq ivy-re-builders-alist
      '((t . re-builder-pinyin)))

(defun user/pinyinlib-build-regexp-string (str)
  (progn
    (cond ((equal str ".*")
           ".*")
          (t
           (pinyinlib-build-regexp-string str t)))))

(defun user/pinyin-regexp-helper (str)
  (cond ((equal str " ")
         ".*")
        ((equal str "")
         nil)
        (t
         str)))

(defun pinyin-to-utf8 (str)
  (cond ((equal 0 (length str))
         nil)
        ((equal (substring str 0 1) "!")
         (mapconcat 'user/pinyinlib-build-regexp-string
                    (remove nil (mapcar
                                 'user/pinyin-regexp-helper
                                 (split-string
                                  (substring str 1) ""))) "")) nil))

(provide 'the-pinyin)
