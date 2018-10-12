    Inside Emacs, in a buffer using AUCTeX, do C-h v TeX-auto-private and create the directory corresponding to the variable's value (typically, /home/myusername/.emacs.d/auctex/auto).

    Run M-x TeX-auto-generate. At the first prompt, give a directory containing your personal class and style files you would like AUCTeX to know about. For instance, if your own_beamer.cls lives in /home/myusername/texmf/tex/latex/MyName, then you could give this directory, but choosing /home/myusername/texmf/tex would also work because the search is recursive (of course, it would probably find more files). At the second prompt, accept the default (typically, ~/.emacs.d/auctex/auto).

    This is going to create /home/myusername/.emacs.d/auctex/auto/own_beamer.el, and similar .el files for your other personal .sty, .cls, etc. files found under the directory you indicated at the first prompt.

    Visit an AUCTeX-using buffer having \documentclass{own_beamer} and type C-c C-n inside that buffer (or just restart Emacs).



Use this command at the command prompt to find out where:

kpsewhich -var-value=TEXMFHOME

On my computer it shows

C:/Users/stefan/texmf

but it might also be ~/texmf/ on a Linux or Unix computer.

Following the TeX directory structure, you should place your file in a subdirectory like ~/texmf/tex/latex/commonstuff/, according to Arthur's comment below. This has the advantage that it is not necessary to update the package database as TeX searches your personal texmf tree directly. If there is an ls-R file in your home texmf tree you can safely delete it as TeX will not use it anyway. (Note: this assumes your personal tree is on a local file system: users with remotely-mounted home folders may still need to hash.)

Regarding MiKTeX, have a look at the section "Installing sty or cls files" in the answer to the question How can I manually install a package on MikTex (Windows).

You can then verify what file will be used with:

kpsewhich filename.sty

This will show the path to the file picked up by the TeX implementation.
Install it "by hand" from CTAN.

Save the two files cookybooky.dtx and cookybooky.ins in a temporary directory and then run latex  cookybooky.ins which creates the documentation and, of course, the style file cookybooky.sty
The doc is obtained with pdflatex cookybooky.dtx
http://www.tug.org/texlive/doc/texlive-en/texlive-en.html
