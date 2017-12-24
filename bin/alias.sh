# *nix programs
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
    alias ll='ls -1a'
    alias llg='ls -1a | grep'
    alias ln='gln'
    alias ls='ls -a'
    alias lsg='ls -a | grep'
    alias lv='ls -la'
    alias lvg='ls -la | grep'
    alias mv='gmv'
    alias pwd='gpwd'
    alias shred='gshred'
    alias sort='gsort'
    alias sum='gsum'
    alias touch='gtouch'
    alias uniq='guniq'
    alias xargs='gxargs'
else
    alias alg='alias | grep '
    alias ll='ls -1aF'
    alias llg='ls -1aF | grep '
    alias ls='ls -aF'
    alias lsg='ls -aF | grep '
    alias lv='ls -laF'
    alias lvg='ls -laF | grep '
fi

alias ff='fzf'
alias nfet='neofetch'
alias tx='tmux'
alias txa='tmux attach -t'
alias txls='tmux ls'
alias txn='tmux new -s'
alias u='cd ..'
alias v='nvim'
alias vi='nvim'

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
fi

# lang
alias bpy='bpython; bpython3'
alias pip='pip3'
alias py='python3'
alias python='python3'

# macOS
if [[ `uname -s` == 'Darwin' ]]
then
    alias em='open -a Emacs'
    alias emacs='open -a Emacs'
    alias ted='open -a TextEdit'
fi
