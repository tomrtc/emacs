# emacs

Personal emacs/shell/machine configuration.

WIP to merge my old elisp with modern req-package.

# install resources #

cp -r fonts ~/.fonts

cp gdbinit ../.gdbinit

mkdir dicts
touch /home/remy/dicts/history.dot


# Using apt on Debian ≥10 and derivatives #

`sudo apt install gdb gdb-doc gdbserver


apt install build-essential


apt install libboost-all-dev


sudo apt install cmake cmake-doc ninja-build cmake-qt-gui cmake-extras


apt install zsh


apt install firmware-iwlwifi


apt install sox imagemagick texlive-full`



# Irony mode clang/client/server with cmake cooperation. #

`sudo apt install elpa-irony`

(add-hook 'c++-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
