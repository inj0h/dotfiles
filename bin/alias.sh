#!/bin/bash
#
# Filename:         alias.sh
# Description:
#                   Useful aliases for a smooth CLI experience.
#

#
# Coreutils
#

if [[ `uname -s` == 'Darwin' ]]; then
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
    alias l='gls -A --color --group-directories-first'
    alias lg='gls -A --color --group-directories-first | grep'
    alias ll='gls -1A --color --group-directories-first'
    alias llg='gls -1A --color --group-directories-first | grep'
    alias ln='gln'
    alias ls='gls'
    alias lv='gls -lA --color --group-directories-first'
    alias lvg='gls -lA --color --group-directories-first | grep'
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
    alias l='ls -A --color --group-directories-first'
    alias lg='ls -A --color --group-directories-first | grep'
    alias ll='ls -1A --color --group-directories-first'
    alias llg='ls -1A --color --group-directories-first | grep'
    alias lv='ls -lA --color --group-directories-first'
    alias lvg='ls -lA --color --group-directories-first | grep'
fi

#
# Git
#

alias g='git'
alias ga='git add'
alias gb='git branch'
alias gbv='git branch | nvim -MR -'
alias gc='git commit'
alias gch='git checkout'
alias gchp='git checkout -p'
alias gcm='git commit -m'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdn='git diff --name-only'
alias gdv='git diff | nvim -MR -'
alias gl='git log'
alias glf='git log --follow --'
alias glv='git log | nvim -MR -'
alias gr='cd $(git rev-parse --show-toplevel)'
alias gs='git status'
alias gsh='git show'
alias gst='git stash'

# Gist
if [[ `uname -s` == 'Linux' ]]; then
    alias gist='gist-paste'
    alias gistp='gist-paste -p'
fi

#
# (Neo)vim
#

alias vf='nvim $(fuss)'
alias vi='nvim'
alias v='nvim'

#
# Etc
#

alias ff='fzf'
alias nfet='neofetch'
alias txa='tmux attach -t'
alias txls='tmux ls'
alias txn='tmux new -s'
alias tx='tmux'
alias u='cd ..'
