# ~/.bashrc
# ------------------------------------------------------------------------------
# Prompt.
export PS1="\u @ \h \w\n$ "

# Terminal type.
export TERM='xterm-256color'

export CLICOLOR=1
export LSCOLORS=GxFxDxBxCxegedabagacad
# Order of lscolors from left to right.
# directory = G
# symlink = F
# socket = D
# pipe = B
# executable = C
# B = red
# C = green
# D = brown
# F = magenta
# G = cyan

# Aliases
# ------------------------------------------------------------------------------
shopt -s expand_aliases                                                 # Vim reads aliases.

# General
alias ls='ls -aGh'
alias vi='/usr/local/opt/neovim/bin/nvim'                               # Neovim.
alias vim='/usr/local/opt/neovim/bin/nvim'                              # And Neovim.

# Apple Scripts
alias goto_safari='cd; cd .bin/appl/; osascript goto_safari.scpt'

# Utility Scripts

# ssh
alias compute='ssh -Y eric.chung@compute.cse.tamu.edu'                  # Compute Server.
alias linux='ssh -Y eric.chung@linux.cse.tamu.edu'                      # Linux Server.

# Git
alias git_shove='./.bin/bash/git_shove.sh'                              # Quick commit.
alias git_up_subs='git submodule foreach git pull origin master'        # Update submodules.
