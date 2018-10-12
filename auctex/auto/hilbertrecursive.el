(TeX-add-style-hook
 "hilbertrecursive"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("preview" "active" "tightpage")))
   (TeX-run-style-hooks
    "latex2e"
    "tufte-handout"
    "tufte-handout10"
    "tikz"
    "preview")
   (TeX-add-symbols
    "HilbertLastX"
    "HilbertLastY"
    "DrawToNext"
    "Hilbert"
    "hilbert"
    "scalefac")
   (LaTeX-add-counters
    "HilbertOrder"))
 :latex)

