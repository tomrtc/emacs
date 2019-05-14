set history save on
set print pretty on
set pagination off
set confirm off

## recall GDB is ptrace() based, when the target process (inferior process in gdb doc)  receives a signal then ptracer process gdb getinfo by waitpid().

## SIGTRAP is generated upon breakpoints and single steping.

## use info signals to have all signals treatement.
## If the target use signals be carefull on which are passed to the target!!

## either gdb --tui or [Ctrl-X] [Ctrl-A] to enter TUI.

## for thraeds
# thread apply all backtrace all

#dynamic printf dprintf
# pour les cas ou on veut un printf-like dans le code!
# dprintf is usable without recompiling the code

## c++ catch exceptions
# catch catch
# catch syscall open

## compile with -ggdb3 more debug info
## old and use to be true but false conception -O and -g are compatible; the result are just sometimes complicated (du to code motion during optimisation).
