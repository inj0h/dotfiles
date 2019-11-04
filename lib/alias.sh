#!/bin/sh
#
# Filename: alias.sh
# Note:     Useful aliases for a smooth CLI experience.
#

#
# Coreutils, GNU stuff for Mac, Etc
#
if [ "$(uname -s)" = "Darwin" ]; then
    alias cat='gcat'
    alias chmod='gchmod'
    alias chown='gchown'
    alias chroot='gchroot'
    alias cp='gcp'
    alias cut='gcut'
    alias echo='gecho'
    alias find='gfind'
    alias grep='ggrep'
    alias head='ghead'
    alias kill='gkill'
    alias ln='gln'
    alias ls='gls'
    alias mv='gmv'
    alias pwd='gpwd'
    alias shred='gshred'
    alias sort='gsort'
    alias sum='gsum'
    alias touch='gtouch'
    alias uniq='guniq'
    alias xargs='gxargs'
fi

#
# Git
#
alias groot='cd $(git rev-parse --show-toplevel)'
alias gsroot='cd $(git rev-parse --show-superproject-working-tree)'

# Gist

if [ "$(uname -s)" = "Linux" ]; then
    alias gist='gist-paste'
    alias gistp='gist-paste -p'
fi

#
# Emacs
#
alias em='emacsclient'
