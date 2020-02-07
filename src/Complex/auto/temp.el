(TeX-add-style-hook
 "temp"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "amsmath"
    "tikz"
    "hyperref"))
 :latex)

