(defun user/clojure-indent-setup ()
  (let ((syms '(a abbr address altGlyph altGlyphDef altGlyphItem animate animateColor animateMotion animateTransform area
  article aside audio b base bdi bdo big blockquote body br button canvas caption circle cite clipPath code
  col colgroup color-profile cursor data datalist dd defs del desc details dfn dialog discard div dl dt
  ellipse em embed feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting
  feDisplacementMap feDistantLight feDropShadow feFlood feFuncA feFuncB feFuncG feFuncR feGaussianBlur
  feImage feMerge feMergeNode feMorphology feOffset fePointLight feSpecularLighting feSpotLight feTile feTurbulence
  fieldset figcaption figure filter font font-face font-face-format font-face-name font-face-src font-face-uri
  footer foreignObject form g glyph glyphRef h1 h2 h3 h4 h5 h6 hatch hatchpath head header hkern hr html
  i iframe image img input ins kbd keygen label legend li line linearGradient link main map mark marker mask
  menu menuitem mesh meshgradient meshpatch meshrow meta metadata meter missing-glyph
  mpath nav noscript object ol optgroup option output p param path pattern picture polygon polyline pre progress q radialGradient
  rect rp rt ruby s samp script section select set small solidcolor source span stop strong style sub summary
  sup svg switch symbol table tbody td text textPath textarea tfoot th thead time title tr track tref tspan
  u ul unknown use var video view vkern wbr)))
    (mapcar (lambda (sym)
              (put-clojure-indent
               (intern (concat "dom/" (symbol-name sym)))
               :defn))
            syms)
    (define-clojure-indent
      (re-frame/reg-event-db :defn)
      (re-frame/reg-event-fx :defn)
      (re-frame/reg-sub :defn)
      (re-frame/test :defn)
      (defmutation [2 [:defn]]))))

(provide 'the-clojure-indent)
