(defface rime-preedit-face
  '((((class color) (background dark))
     (:underline t))
    (((class color) (background light))
     (:underline t)))
  "输入法嵌入首选的样式"
  :group 'rime)

(defface rime-indicator-face
  '((((class color) (background dark))
     (:foreground "#9256B4" :bold t))
    (((class color) (background light))
     (:foreground "#9256B4" :bold t)))
  "提示符的样式"
  :group 'rime)

;;; 只要`input-method-function'有定义就会被使用。而启用输入法只生效在当前`buffer'
;;; 所以需要这些变量为`buffer-local'，
(make-variable-buffer-local 'input-method-function)
(make-variable-buffer-local 'deactivate-current-input-method-function)

(defcustom rime--disable-predicates nil
  "当此列表中任何一个断言函数成立时，进入临时英文模式。"
  :type 'list
  :group 'rime)

(defcustom rime--show-candidate t
  "是否在`minibuffer'中显示候选列表。")

(make-variable-buffer-local
 (defvar rime--preedit-overlay nil
   "存储嵌入首选的`overlay'，用于标记其范围便于修改。"))

(make-variable-buffer-local
 (defvar rime--backspace-fallback nil
   "记录之前的`backspace'绑定的函数，在`rime-update-binding'中会被刷新。"))

(make-variable-buffer-local
 (defvar rime--return-fallback nil
   "记录之前的`return'绑定的函数，在`rime-update-binding'中会被刷新。"))

(make-variable-buffer-local
 (defvar rime--enable nil
   "当前`buffer'中是否激活输入法。"))

(make-variable-buffer-local
 (defvar rime--prev-preedit nil
   "之前的`preedit'，在`liberime'中如果出现空码，状态会被清空。保存之前的`preedit'用于在空码的情况下进行恢复。"))

(defun rime--after-alphabet-char-p ()
  "当前光标是否在英文的后面。"
  (when (char-before)
    (string-match-p "[a-zA-Z-_]" (char-to-string (char-before)))))

(defun rime--prog-in-code-p ()
  "当前为`prog-mode'或`conf-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun rime--should-enable-p ()
  (not (seq-find 'funcall rime--disable-predicates)))

(defun rime--show-candidates ()
  (when rime--show-candidate
    (let* ((context (liberime-get-context))
           (candidates (alist-get 'candidates (alist-get 'menu context)))
           (preedit (alist-get 'preedit (alist-get 'composition context)))
           (idx 1)
           (result ""))
      (when context
        (setq result
              (concat result (format "%s ┃ " preedit)))
        (dolist (c candidates)
          (setq result
                (concat result (format "%d. %s " idx c)))
          (setq idx (1+ idx))))
      (message result))))

(defun rime--clear-overlay ()
  (when (overlayp rime--preedit-overlay)
    (delete-region (overlay-start rime--preedit-overlay) (overlay-end rime--preedit-overlay))
    (delete-overlay rime--preedit-overlay)
    (setq rime--preedit-overlay nil)))

(defun rime--display-preedit ()
  (let ((preedit (alist-get 'commit-text-preview (liberime-get-context))))
    ;; Always delete the old overlay.
    (rime--clear-overlay)
    ;; Create the new preedit
    (when preedit
      (let ((beg (point)))
        (insert preedit)
        (setq rime--preedit-overlay (make-overlay beg (point)))
        (overlay-put rime--preedit-overlay 'face 'rime-preedit-face)))))

(defun rime--backspace ()
  (interactive)
  (if (not (rime--should-enable-p))
      (when rime--backspace-fallback
        (call-interactively rime--backspace-fallback))
    (let ((context (liberime-get-context)))
      (if (not context)
          (call-interactively rime--backspace-fallback)
        (liberime-process-key 65288)
        (rime--show-candidates)
        (rime--display-preedit)
        (setq-local rime--prev-preedit
              (thread-last (liberime-get-context)
                (alist-get 'composition)
                (alist-get 'preedit)))))))

(defun rime--return ()
  "回车的行为：
1. 在临时英文模式下，使用`rime--return-fallback'。
2. 如果有`preedit'，则上屏并清空状态。
3. 清空状态并调用`rime--return-fallback'"
  (interactive)
  (if (not (rime--should-enable-p))
      (when rime--return-fallback
        (call-interactively rime--return-fallback))
    (let ((preedit (thread-last (liberime-get-context)
                     (alist-get 'composition)
                     (alist-get 'preedit))))
      (if preedit
          (progn
            (rime--clear-overlay)
            (insert preedit)
            (setq-local rime--prev-preedit nil)
            (liberime-clear-composition))
        (liberime-clear-composition)
        (setq-local rime--prev-preedit nil)
        (rime--show-candidates)
        (rime--clear-overlay)
        (call-interactively rime--return-fallback)))))

(defun rime-input-method (key)
  (if (and (not (rime--should-enable-p))
           (not (liberime-get-context)))
      (list key)
    (liberime-process-key key)
    (with-silent-modifications
      (let* ((context (liberime-get-context))
             (preedit (thread-last context
                        (alist-get 'composition)
                        (alist-get 'preedit)))
             (commit (liberime-get-commit)))
        (unwind-protect
            (cond
             ((and (not context) (not commit) (not preedit))
              (if rime--prev-preedit
                  (progn
                    (liberime-clear-composition)
                    (dolist (c (mapcar 'identity rime--prev-preedit))
                      (liberime-process-key c)
                      (rime--show-candidates)
                      (rime--display-preedit))
                    (setq preedit
                          (thread-last (liberime-get-context)
                            (alist-get 'composition)
                            (alist-get 'preedit))))
                (liberime-clear-composition)
                (list key)))
             (commit
              (rime--clear-overlay)
              (mapcar 'identity commit))
             (t (rime--show-candidates)
                (rime--display-preedit)))
          (setq-local rime--prev-preedit preedit))))))

(defun rime--clean-state ()
  "清空状态，包换`liberime'的状态和`preedit'。"
  (liberime-clear-composition)
  (when (overlayp rime--preedit-overlay)
    (delete-overlay rime--preedit-overlay)
    (setq-local rime--preedit-overlay nil))
  (setq-local rime--prev-preedit nil))

(defun rime-update-binding ()
  "更新输入法的按键绑定，记录原本`RET'和`DEL'的功能。"
  (if rime--enable
      (progn
        (rime--clean-state)
        (rime-mode -1)
        (setq-local rime--backspace-fallback (key-binding (kbd "DEL")))
        (setq-local rime--return-fallback (key-binding (kbd "RET")))
        (rime-mode 1))
    (progn
      (set-input-method nil)
      (setq-local rime--backspace-fallback nil)
      (setq-local rime--return-fallback nil)
      (rime-mode -1))))

(defun rime-register-and-set-default ()
  "注册 RIME 输入法并设置为默认的方案。"
  (register-input-method "rime" "euc-cn" 'rime-activate "ㄓ")
  (setq-default default-input-method 'rime))

(defun rime-toggle ()
  "激活 RIME 输入法。"
  (interactive)
  (if current-input-method
      (set-input-method nil)
    (set-input-method 'rime)))

(defun rime-select-schema ()
  "选择 RIME 中使用的方案。"
  (interactive)
  (let* ((schema-list (liberime-get-schema-list))
         (schema-names (mapcar 'cdr schema-list))
         (schema-name (completing-read "Schema: " schema-names))
         (schema (thread-last schema-list
                   (seq-find (lambda (s)
                               (message "%s %s" (cdr s) schema-name)
                               (equal (cadr s) schema-name)))
                   (car))))
    (message "Rime schema: %s" schema-name)
    (liberime-select-schema schema)))

(defun rime-lighter ()
  "返回一个可以用于展示在`modeline'的ㄓ符号。"
  (if rime--enable
      (propertize
       " ㄓ"
       'face
       'rime-indicator-face)
    ""))

(defun rime-activate (name)
  (setq input-method-function 'rime-input-method
        deactivate-current-input-method-function #'rime-deactivate)
  (liberime-clear-composition)
  (setq-local rime--enable t)
  (setq-local rime--prev-preedit nil)
  (rime-update-binding)
  (rime-mode 1)
  (message "Rime activate."))

(defun rime-deactivate ()
  (rime--clean-state)
  (setq-local rime--enable nil)
  (rime-mode -1)
  (message "Rime deactivate."))

(defvar rime-mode-map
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "DEL") 'rime--backspace)
        (define-key keymap (kbd "RET") 'rime--return)
        keymap))

(defun rime-mode--init ()
  (unless rime--backspace-fallback
    (setq-local rime--backspace-fallback (key-binding (kbd "DEL"))))
  (unless rime--return-fallback
    (setq-local rime--return-fallback (key-binding (kbd "RET"))))
  (add-hook 'post-self-insert-hook 'rime--display-preedit nil t)
  (add-hook 'post-self-insert-hook 'rime--show-candidates nil t))

(defun rime-mode--uninit ()
  (remove-hook 'post-self-insert-hook 'rime--display-preedit)
  (remove-hook 'post-self-insert-hook 'rime--show-candidates))

(define-minor-mode rime-mode
  "仅用于提供输入法激活状态下的按键绑定。

该模式不应该被手动启用。"
  nil
  nil
  rime-mode-map
  (if rime-mode
      (rime-mode--init)
    (rime-mode--uninit)))

(provide 'rime)
