# ~/.profile
# ------------------------------------------------------------------------------
# Welcome message
printf "New session with $USER! \xf0\x9f\x8e\xb8  \n\n"

neofetch

# Set shell to zsh (MacOS doesn't default to this).
if [ -f ~/.zshrc ]; then
    source ~/.zshrc
fi
