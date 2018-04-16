# filename:         zprofile
# description:
#                   Personal profile for zshell.
#

# exports
export VISUAL=vim
export EDITOR="$VISUAL"

# welcome message
printf "New session with $USER!\n"

# source .zshrc for login shells
if [ -f ~/.zshrc ]; then
    source ~/.zshrc
fi
