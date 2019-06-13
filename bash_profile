# Filename: bash_profile
# Note:     Just your average bash_profile.
#

#
# Variables
#
export VISUAL=vi
export EDITOR="$VISUAL"

# Welcome Message
printf "New session with $USER!\n"

# Externalities
#
# Completion
[[ `uname -s` == 'Linux' ]] &&
    . /etc/profile.d/bash_completion.sh

# Fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_OPTS='--color bg:-1,bg+:-1,fg+:-1'

command -v rg > /dev/null &&
    export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'

# Haskell
#
# Stack
[[ `uname -s` == 'Linux' ]] &&
    export PATH=$PATH:/home/echung/.local/bin

# Load the configs!
[ -f ~/.bashrc ] && . ~/.bashrc
[ -f ~/.dir_colors/dircolors ] && eval `dircolors ~/.dir_colors/dircolors`
