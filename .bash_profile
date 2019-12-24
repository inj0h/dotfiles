# Filename: bash_profile
# Note:     Just your average bash_profile.

#
# System Variables
#

# Editor
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# History
HISTCONTROL=ignoreboth
HISTSIZE=100000


# Welcome Message
printf "New session with %s!\n" "$USER"

# Load the configuration file.
[ -r ~/.bashrc ] && . ~/.bashrc
