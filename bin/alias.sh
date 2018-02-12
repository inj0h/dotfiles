#!/bin/bash
#
# filename:         alias.sh
# description:
#                   Useful aliases for a smooth CLI experience.
#
# ---------------------------------------------------------------------------- #

# coreutils
if [[ `uname -s` == 'Darwin' ]]
then
    alias alg='alias | grep '
    alias cat='gcat'
    alias chmod='gchmod'
    alias chown='gchown'
    alias chroot='gchroot'
    alias cp='gcp'
    alias cut='gcut'
    alias echo='gecho'
    alias find='gfind'
    alias grep='grep --color'
    alias head='ghead'
    alias kill='gkill'
    alias llg='ls -1A | grep'
    alias ll='ls -1A'
    alias ln='gln'
    alias lsg='ls -A | grep'
    alias ls='ls -A'
    alias lvg='ls -lA | grep'
    alias lv='ls -lA'
    alias mv='gmv'
    alias pwd='gpwd'
    alias shred='gshred'
    alias sort='gsort'
    alias sum='gsum'
    alias touch='gtouch'
    alias uniq='guniq'
    alias xargs='gxargs'
else
    alias alg='alias | grep'
    alias llg='ls -1A --color | grep'
    alias ll='ls -1A --color'
    alias lsg='ls -A --color | grep'
    alias ls='ls -A --color'
    alias lvg='ls -lA --color| grep'
    alias lv='ls -lA --color'
fi

alias ff='fzf'
alias nfet='neofetch'
alias txa='tmux attach -t'
alias txls='tmux ls'
alias txn='tmux new -s'
alias tx='tmux'
alias u='cd ..'
alias vi='nvim'
alias v='nvim'

# git
alias g='git'
alias ga='git add'
alias gb='git branch'
alias gch='git checkout'
alias gcm='git commit -m'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdl='git diff | less'
alias gl='git log'
alias gs='git status'
alias gsh='git show'
alias gst='git stash'

# gist
if [[ `uname -s` == 'Linux' ]]
then
    alias gist='gist-paste'
    alias gistp='gist-paste -p'
fi

# macOS
if [[ `uname -s` == 'Darwin' ]]
then
    alias emacs='open -a Emacs'
    alias em='open -a Emacs'
    alias ted='open -a TextEdit'
fi
