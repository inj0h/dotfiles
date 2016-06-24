# ~/.profile
# ------------------------------------------------------------------------------ 
printf "New session with $USER! \xf0\x9f\x8e\xb7  \n"                            # welcome message 

# set shell to zsh (macOS doesn't default to this)
if [ -f ~/.zshrc ]; then
    source ~/.zshrc
fi
