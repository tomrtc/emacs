(TeX-add-style-hook
 "tufte-book"
 (lambda ()
   (TeX-run-style-hooks
    "tufte-common"
    "tufte-book-local"))
 :latex)

