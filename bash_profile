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

# Load the configs!
#
# In case we don't run a login shell every time.
[ -f ~/.bashrc ] && . ~/.bashrc

[ -f ~/.dir_colors/dircolors ] && eval `dircolors ~/.dir_colors/dircolors`
