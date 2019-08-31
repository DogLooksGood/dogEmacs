;;; alabaster-theme.el --- Alabaster theme for Emacs.

;; Copyright (C) 2019-2019 James Lin Taylor

;; Author: James Lin Taylor <jameslintaylor@gmail.com>
;; URL: https://github.com/this-does-not-have-a-url-yet
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme alabaster
  "Alabaster theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))

       (active     "#007ACC")
       (fg         "#000")
       (bg         "gray95")
       (blue       "#DBF1FF")
       (green      "#F1FADF")
       (dark-green "#DBECB6")
       (red        "#FFE0E0")
       (magenta    "#F9E0FF")
       (yellow     "#FFFABC")
       (orange     "#FFBC5D")

       )

  (custom-theme-set-faces
   'alabaster

   `(default ((,class (:background ,bg :foreground ,fg))))

   ;; global font lock
   `(font-lock-keyword-face       ((,class (:foreground ,fg))))
   `(font-lock-constant-face      ((,class (:foreground ,fg))))
   `(font-lock-type-face          ((,class (:foreground ,fg))))
   `(font-lock-builtin-face       ((,class (:background "gray90"))))
   `(font-lock-string-face        ((,class (:background ,dark-green))))
   `(font-lock-doc-face           ((,class (:background ,yellow))))
   `(font-lock-comment-face       ((,class (:background ,yellow))))

   `(font-lock-function-name-face ((,class (:background ,blue))))
   `(font-lock-variable-name-face ((,class (:background ,blue))))

   ;; clojure font lock
   `(clojure-keyword-face         ((,class (:background "gray90"))))

   ;; mode line
   `(mode-line                    ((,class (:inverse-video t))))
   `(mode-line-inactive           ((,class (:underline t))))

   ;; hl-line
   `(hl-line                      ((,class (:background ,red))))))

(provide-theme 'alabaster)

;;; alabaster-theme.el ends here
