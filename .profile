# ~/.profile
# ------------------------------------------------------------------------------ 
printf "Welcome $USER! \xf0\x9f\x98\x84  \n"                            # welcome message 

# set shell to zsh (macOS doesn't default to this)
if [ -f ~/.zshrc ]; then
   source ~/.zshrc
fi
