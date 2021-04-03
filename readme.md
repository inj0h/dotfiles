# Dotfiles
My programming configuration files. Unix. Emacs. _Etc._ Share. Steal. Blah,
blah, blah. To get the most out of these files, it would help to do your
computing on a Unix system. Not tested on Windows (YMMV).

## Emacs
I keep a literate configuration with `dotemacs.org`, and use `bootstrap.el` to
output a __timestamped Elisp file,__ which I can then symlink against. The
timestamped Elisp file gets committed to this repository where it hopefully
serves as a backup (hopefully).

``` shell
$ emacs --script bootstrap.el
$ ln -s /PATH/TO/TIMESTAMPPED/CONFIG ~/.config/emacs/init.el
```
