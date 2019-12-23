# Filename: bash_profile
# Note:     Just your average bash_profile.

#
# Variables
#
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# Welcome Message
printf "New session with $USER!\n"

# Load the configs!
#
# In case we don't run a login shell every time.
[ -f ~/.bashrc ] && . ~/.bashrc
