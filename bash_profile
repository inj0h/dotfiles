# Filename:    bash_profile
# Description: Just your average bash_profile.
#

#
# Variables
#
export VISUAL=vi
export EDITOR="$VISUAL"

# Welcome Message
printf "New session with $USER!\n"

# Load the config!
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
