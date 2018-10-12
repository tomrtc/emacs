(TeX-add-style-hook
 "template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "titlepage" "12pt")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "fontspec"
    "lipsum"
    "fullpage"
    "nameref"
    "textcomp"
    "titling"
    "coffee"
    "fancyhdr"
    "geometry")
   (TeX-add-symbols
    "COFFEESTAINGRAYSCALE"))
 :latex)

