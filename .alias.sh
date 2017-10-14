# *nix programs
alias alg='alias | grep '
alias ll='ls -1aF'
alias llg='ls -1aF | grep '
alias ls='ls -aF'
alias lsg='ls -aF | grep '
alias lv='ls -laF'
alias lvg='ls -laF | grep '
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
alias g='git'
alias ga='git add'
alias gb='git branch'
alias gch='git checkout'
alias gcm='git commit -m'
alias gd='git diff'
alias gdl='git diff | less'
alias gs='git status'
alias gsh='git show'

# Python
alias py='python3'
alias bpy='bpython3'
