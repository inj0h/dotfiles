# Dotfiles
## About
My Unix configuration files. Sort of. Share, steal, _etc._

## System Compatibility
To get the most out of these files, you need to do your computing on a Unix
system. Not guaranteed to work on Windows. WSL might help.

## Installation
Clone this repository and run the `connect-the-dots` POSIX shell script to link
the files to your home directory.

``` shell
$ git clone this-repository-url
$ cd path-to-cloned-repository-on-disk
$ ./bin/connect-the-dots --link dotfiles.txt
```

### Configuration File
The script will read the contents of `dotfiles.txt` in order to determine the
paths. __You can configure the plain text file__ to link your own dotfiles -
useful if you keep a lot of them around. For more details, run
`connect-the-dots` with the `--help` flag.

``` shell
$ ./bin/connect-the-dots --help
```
