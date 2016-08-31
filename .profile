# ~/.profile
# ------------------------------------------------------------------------------
# Welcome Message
printf "New session with $USER! \xf0\x9f\x8e\xb7  \n"

# Adding to $PATH
export PATH=~/anaconda2/bin:"$PATH"

# Set shell to zsh (MacOS doesn't default to this).
if [ -f ~/.zshrc ]; then
    source ~/.zshrc
fi
