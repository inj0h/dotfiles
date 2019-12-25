# Filename: bash_profile
# Note:     Just your average bash_profile.

# Editor
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# History
HISTCONTROL=ignoreboth
HISTSIZE=100000

# Welcome Message
printf "New session with %s!\n" "$USER"

[ -r ~/.bashrc ] && . ~/.bashrc
