# ~/.bashrc
# ------------------------------------------------------------------------------
# Prompt
export PS1="\u @ \h \w\n$ "

# Terminal type
export TERM='xterm-256color'

# Ls colors
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
shopt -s expand_aliases                                     # Vim reads aliases.

# Source my aliases.
if [ -f ~/.my_aliases ];
then
    source ~/.my_aliases
fi

# ssh
if [ -f ~/.ssh_aliases ];
then
    source ~/.ssh_aliases
fi
