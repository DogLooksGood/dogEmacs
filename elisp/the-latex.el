;; (setq org-latex-default-packages-alist
;;       '(("" "hyperref" nil)
;;         ("AUTO" "inputenc" t)
;;         ("" "fixltx2e" nil)
;;         ("" "graphicx" t)
;;         ("" "longtable" nil)
;;         ("" "float" nil)
;;         ("" "wrapfig" nil)
;;         ("" "rotating" nil)
;;         ("normalem" "ulem" t)
;;         ("" "amsmath" t)
;;         ("" "textcomp" t)
;;         ("" "marvosym" t)
;;         ("" "wasysym" t)
;;         ("" "multicol" t)  ; 這是我另外加的，因為常需要多欄位文件版面。
;;         ("" "amssymb" t)
;;         "\\tolerance=1000"))

;; Use XeLaTeX to export PDF in Org-mode
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))



(provide 'the-latex)
