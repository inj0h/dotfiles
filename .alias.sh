# *nix programs
alias alg='alias | grep '
alias llg='ls -1aF | grep '
alias ll='ls -1aF'
alias lsg='ls -aF | grep '
alias ls='ls -aF'
alias lvg='ls -laF | grep '
alias lv='ls -laF'
alias txa='tmux attach -t'
alias txls='tmux ls'
alias txn='tmux new -s'
alias u='cd ..'
alias v='nvim'

# gist
if [[ `uname` == 'Linux' ]]
then
    alias gist='gist-paste'
fi

# git
alias ga='git add'
alias gb='git branch'
alias gch='git checkout'
alias gcm='git commit -m'
alias gdc='git diff --cached'
alias gd='git diff'
alias gdl='git diff | less'
alias g='git'
alias gl='git log'
alias gs='git status'
alias gsh='git show'


# Python
alias bpy='bpython3'
alias py='python3'
