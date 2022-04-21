# Mostly Emacs
But also Bash, ripgrep, and some other stuff. I use these files to do most of my programming, usually on some Unix environment.

Maybe Windows one day.

## Emacs
I keep a literate configuration with `dotemacs.org`, and use `bootstrap.el` to output a __timestamped Elisp file,__ which I can then symlink against. The timestamped file gets committed to this repository where it hopefully :crossed_fingers: serves as a backup. Also, my config probably works best with Emacs 28 or later these days.

``` shell
$ emacs -batch -l bootstrap.el dotemacs.org
$ "Tangled N code blocks from dotemacs.org"
$ "Success! Tangled: dotemacs.org -> dotemacs_YYYYMMDD.el"
$ ln -s ~/.dotfiles/dotemacs_YYYYMMDD.el ~/.config/emacs/init.el
```
