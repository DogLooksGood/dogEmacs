;;; -*- lexical-binding: t -*-

(straight-use-package 'dash)

(require 'dash)
(require 'subr-x)

(defvar-local +project-name-cache nil
  "Cache for current project name.")

(defun +in-string-p ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (or (nth 3 (syntax-ppss))
      (member 'font-lock-string-face
              (text-properties-at (point)))))

(defun +in-comment-p ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (nth 4 (syntax-ppss)))

(defvar +smart-file-name-cache nil)

(defun +shorten-long-path (path)
  (let ((paths (split-string path "/")))
    (if (< (length paths) 3)
        path
      (string-join (reverse (let ((rpaths (reverse paths)))
                                (-concat
                                 (-take 2 rpaths)
                                 (->> (-drop 2 rpaths)
                                      (--map (if (> (length it) 1)
                                                 (substring it 0 1)
                                               it))))))
                     "/"))))

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (concat
       (propertize
        (car
         (reverse
          (split-string (string-trim-right vc-dir "/") "/")))
        'face
        'bold)
       "/"
       (+shorten-long-path (file-relative-name bfn vc-dir))))
     (bfn bfn)
     (t (buffer-name)))))

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

(defun +vc-branch-name ()
  (when vc-mode
    (propertize
     (replace-regexp-in-string
      "Git[-:]"
      ""
      (substring-no-properties vc-mode))
     'face
     'bold)))

(defmacro +measure-time-1 (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.03fms"
              (* 1000 (float-time (time-since time))))))

(defmacro +measure-time (&rest body)
  "Measure the time it takes to evalutae BODY, repeat 10 times."
  `(let ((time (current-time))
         (n 10))
     (dotimes (_ n),@body)
     (message "%.03fms"
              (/ (* (float-time (time-since time)) 1000) n))))

(defface +modeline-dim-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey50")))
  "Dim face in mode-line")

(defun +make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

;;; Case transform

(defun +to-pascal-case (s)
  (let* ((words (split-string s "-\\|_"))
         (capwords (mapcar #'capitalize words)))
    (string-join capwords "")))

(defun +color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (ignore-errors
    (apply #'(lambda (r g b)
               format "#%02x%02x%02x"
               (ash r -8)
               (ash g -8)
               (ash b -8))
           (cl-mapcar
            (lambda (x y)
              (round (+ (* x alpha) (* y (- 1 alpha)))))
            (color-values c1) (color-values c2)))))

(defun +set-no-other-window ()
  (set-window-parameter (car (window-list)) 'no-other-window t))

(provide 'init-util)
