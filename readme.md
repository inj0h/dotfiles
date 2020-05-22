# Dotfiles

## About
My nix configuration files. Share, steal, _etc._

## System Compatibility
To get the most out of these files, you need to do your computing on a
POSIX system. No idea if these files work on native Windows. WSL might
workout.

## Installation
Clone this repository and run the `connect_the_dots` shell script to
link the files to your home directory.

``` shell
$ git clone this-repository-url
$ cd path-to-cloned-repository-on-disk
$ ./bin/connect_the_dots --link dotfiles.txt
```

### Configuration File
The script will read the contents of `dotfiles.txt` in order to
determine the paths. __You can configure the plain text file__ to link
your own dotfiles - useful if you keep a lot of them around. For more
details, run `connect_the_dots` with the `--help` flag.

``` shell
$ ./bin/connect_the_dots --help
```
