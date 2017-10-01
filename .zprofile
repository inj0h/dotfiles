# ~/.zprofile
# ------------------------------------------------------------------------------
# Welcome message
printf "New session with $USER! \xf0\x9f\x8e\xb8  \n"

# Source .zshrc for login shells
if [ -f ~/.zshrc ]; then
    source ~/.zshrc
fi
