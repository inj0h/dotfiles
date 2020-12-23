# Dotfiles
## About
My Unix configuration files. Emacs. _Etc._ Share, steal, blah, blah, blah.

## System Compatibility
To get the most out of these files, you need to do your computing on a Unix
system. Not tested on Windows (YMMV).

## (Un)installation
Clone this repository, and use the provided `Makefile`, which runs the
`connect-the-dots` shell script to link the files. Note, their destination
depends on the paths set in the `dotfiles.txt` configuration file.

``` sh
$ make
```

Similarly, run `make clean` to uninstall, _i.e._ destroying the symlinks, and
`make help` to get an overview of the commands.

### Configuration File
The script will read the contents of `dotfiles.txt` in order to determine the
paths. __You can configure the plain text file__ to link your own dotfiles -
useful if you keep a lot of them around. For more details, run
`connect-the-dots` with the `--help` flag.

``` sh
$ ./bin/connect-the-dots --help
```
