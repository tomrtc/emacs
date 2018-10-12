(TeX-add-style-hook
 "tufte-handout"
 (lambda ()
   (TeX-run-style-hooks
    "tufte-common"
    "tufte-handout-local"))
 :latex)

