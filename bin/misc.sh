 git
alias gsuri="git submodule update --recursive --init"
# http://stackoverflow.com/a/11366713/677381
alias gignore="git update-index --assume-unchanged"
alias gunignore="git update-index --no-assume-unchanged"
alias glsignore="git ls-files -v | grep \"^[[:lower:]]\""
alias e="emacsclient -t"
alias ew="emacsclient -n -c"
alias se="SUDO_EDITOR='emacsclient -t' sudoedit"
alias see="SUDO_EDITOR='emacsclient' toemacs sudoedit -b $*"
alias sew="SUDO_EDITOR='emacsclient -c' sudoedit -b $*"

alias killemacs="emacsclient -e \"(kill-emacs)\" -a false"
alias compemacs='emacs --batch -l ~/.emacs.d/init.el --eval "(byte-recompile-directory (expand-file-name \"~/.emacs.d\") 0)" --kill'

# cmake
alias cmake="\cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
alias c=cmake
alias cn="cmake -G Ninja"
alias cn-all="cn .. && ninja && ctest --output-on-failure"
alias cm="cmake -G 'Unix Makefiles'"
alias cm-all="cm .. && make -j 4 && ctest --output-on-failure"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"
#
# Utils for manipulating environment paths
#

add-path()
{
    var=$1
    del-path $var $@
    shift
    for path in $@
    do
        declare -gx $var="$path${!var:+":${!var}"}"
    done
}

del-path()
{
    var=$1
    shift
    for path in $@
    do
        declare -gx $var=${!var//":$path:"/:} #delete all instances in the middle
        declare -gx $var=${!var/%":$path"/} #delete any instance at the end
        declare -gx $var=${!var/#"$path:"/} #delete any instance at the beginning
        declare -gx $var=${!var//"$path"/} #delete singleton instance
    done
}
#
# local installations
#
add-path LD_LIBRARY_PATH /usr/local/lib
add-path PATH /usr/local/bin
#
# User installations
#
add-path LD_LIBRARY_PATH "$HOME/usr/lib"
add-path PATH "$HOME/usr/bin"
#
# Make bash history work on multiple tabs and be very big.
# http://unix.stackexchange.com/a/48116
#

HISTSIZE=100000
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignorespace:ignoredups

bash_history_sync() {
    builtin history -a
    HISTFILESIZE=$HISTSIZE
    builtin history -c
    builtin history -r
}

history() {
    bash_history_sync
    builtin history "$@"
}

PROMPT_COMMAND=bash_history_sync
# Make emacs realize it can use colors
if [ "x$EMACS" == "xt" ]; then
    export TERM=eterm-color
elif [ "x$INSIDE_EMACS" != "x" ]; then
    export TERM=eterm-color
fi
